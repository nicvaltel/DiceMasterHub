{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunWebSocketServer (runWebSocketServerMVar, runWebSocketServerTMVar) where

import Control.Concurrent (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar)
import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    atomically,
    newTMVarIO,
    newTQueueIO,
    putTMVar,
    takeTMVar,
    writeTQueue,
  )
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Logger (logger, LgSeverity (LgInfo, LgMessage))
-- import Network.WebSockets (Connection)
import qualified Network.WebSockets as WS
import Types
import Control.Monad.RWS (RWST (runRWST))
import IntMapRepo(IntMapRepo)
import qualified IntMapRepo
import GameRoom (RoomId)

class WebSocketServer (mvar :: Type -> Type) where
  addConnection :: mvar WebSocketServerState -> WS.Connection -> TQueue WSMsgFormat -> IO ConnectionId
  removeConnection :: mvar WebSocketServerState -> ConnectionId -> IO ()
  newContainerVar :: a -> IO (mvar a)

  runWebSocketServer :: String -> Int -> IO (mvar WebSocketServerState)
  runWebSocketServer host port = do
    (state :: mvar WebSocketServerState) <- newContainerVar newWebSocketServerState
    WS.runServer host port $ webSocketServer state
    pure state

  webSocketServer :: mvar WebSocketServerState -> WS.ServerApp
  webSocketServer wSState = \pending -> do
    conn <- WS.acceptRequest pending
    checkForExistingUser conn -- TODO check for user
    connStateMessageQueue <- newTQueueIO :: IO (TQueue WSMsgFormat)
    idConn <- addConnection wSState conn connStateMessageQueue
    logger LgInfo $ show idConn ++ " connected"
    WS.withPingThread conn 30 (pure ()) $ do
      finally 
        (runRWST (wsThreadMessageListner idConn) (ConnThreadReader conn) (ConnThreadState Nothing) >> pure ()) 
        (disconnect idConn)
    where
      disconnect idConn = do
        removeConnection wSState idConn
        logger LgInfo $ show idConn ++ " disconnected"

data WebSocketServerState = WebSocketServerState
  { wsConnectionRepo :: IntMapRepo ConnectionState
  }

data ConnectionState = ConnectionState
  { connStateConnection :: WS.Connection,
    connStateMessageQueue :: TQueue WSMsgFormat
  }

type ConnectionId = Int



newtype ConnThreadReader = ConnThreadReader WS.Connection
data ConnThreadState = ConnThreadState {connStateRoom :: Maybe RoomId}
type ConnThreadWriter = Text

type ConnThread a = RWST ConnThreadReader ConnThreadWriter ConnThreadState IO a

newWebSocketServerState :: WebSocketServerState
newWebSocketServerState = WebSocketServerState  { wsConnectionRepo = IntMapRepo.empty  }

addConnToState :: WS.Connection -> TQueue WSMsgFormat -> WebSocketServerState -> (WebSocketServerState, ConnectionId)
addConnToState conn connStateMessageQueue wssState = 
  let repo = wsConnectionRepo wssState
      connState = ConnectionState{connStateConnection = conn, connStateMessageQueue}
      (newRepo, idConn) = IntMapRepo.append connState repo
   in (WebSocketServerState{wsConnectionRepo = newRepo}, idConn)
  
  
removeConnFromState :: ConnectionId -> WebSocketServerState -> WebSocketServerState
removeConnFromState idConn wssState = 
  let repo = wsConnectionRepo wssState
      newRepo = IntMapRepo.delete idConn repo
   in WebSocketServerState{wsConnectionRepo = newRepo}

checkForExistingUser :: WS.Connection -> IO ()
checkForExistingUser _ = pure ()

instance WebSocketServer TMVar where
  addConnection :: TMVar WebSocketServerState -> WS.Connection -> TQueue WSMsgFormat -> IO ConnectionId
  addConnection tmvWSState conn connStateMessageQueue = atomically $ do
    wssState <- takeTMVar tmvWSState
    let (newWssState, idConn) = addConnToState conn connStateMessageQueue wssState
    putTMVar tmvWSState newWssState
    pure idConn

  removeConnection :: TMVar WebSocketServerState -> ConnectionId -> IO ()
  removeConnection tmvWSState idConn = atomically $ do
    wssState <- takeTMVar tmvWSState
    let newWssState = removeConnFromState idConn wssState
    putTMVar tmvWSState newWssState

  newContainerVar :: a -> IO (TMVar a)
  newContainerVar = newTMVarIO

instance WebSocketServer MVar where
  addConnection :: MVar WebSocketServerState -> WS.Connection -> TQueue WSMsgFormat -> IO ConnectionId
  addConnection mvWSState conn connStateMessageQueue = modifyMVarMasked mvWSState (pure . addConnToState conn connStateMessageQueue)

  removeConnection :: MVar WebSocketServerState -> ConnectionId -> IO ()
  removeConnection mvWSState idConn = modifyMVarMasked_ mvWSState (pure . removeConnFromState idConn)

  newContainerVar :: a -> IO (MVar a)
  newContainerVar = newMVar

wsThreadMessageListner :: ConnectionId -> ConnThread ()
wsThreadMessageListner idConn = forever $ do
  ConnThreadReader conn <- ask
  (msg :: WSMsgFormat) <- liftIO $ WS.receiveData conn
  liftIO $ logger LgInfo $ "RECIEVE (#" <> show idConn <> "): " <> Text.unpack msg
  case (toWebSocketInputMessage msg) of
    LogInOutMsg logMsg -> processMsgLogInOut logMsg
    InitJoinRoomMsg ijrMsg -> processInitJoinRoom ijrMsg
    GameActionMsg gameActMsg -> processGameActionMsg gameActMsg
    IncorrectMsg txts -> processIncorrectMsg txts

processMsgLogInOut :: LogInOut -> ConnThread ()
processMsgLogInOut (Login username) = undefined
processMsgLogInOut Logout = undefined

processInitJoinRoom :: InitJoinRoom -> ConnThread ()
processInitJoinRoom (InitGameRoom params) = sendWebSocketOutputMessage RoomWaitingForParticipantMsg
processInitJoinRoom (JoinGameRoom roomId) = undefined

processGameActionMsg :: GameAction -> ConnThread ()
processGameActionMsg (GameAction params) = undefined

processIncorrectMsg :: [Text] -> ConnThread ()
processIncorrectMsg _ = sendWebSocketOutputMessage ResendIncorrectMsg

sendWebSocketOutputMessage :: WebSocketOutputMessage -> ConnThread ()
sendWebSocketOutputMessage msg = do
  liftIO $ logger LgMessage (Text.unpack $ fromWebSocketOutputMessage msg)
  ConnThreadReader conn <- ask
  liftIO $ WS.sendTextData conn (fromWebSocketOutputMessage msg :: Text)

runWebSocketServerMVar :: String -> Int -> IO ()
runWebSocketServerMVar host port = (runWebSocketServer host port :: IO (MVar WebSocketServerState)) >> pure ()

runWebSocketServerTMVar :: String -> Int -> IO ()
runWebSocketServerTMVar host port = (runWebSocketServer host port :: IO (TMVar WebSocketServerState)) >> pure ()