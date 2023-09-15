-- websocat -v ws://127.0.0.1:1234

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.RunWebSocketServer (runWebSocketServerMVar, runWebSocketServerTMVar) where

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
-- import Network.WebSockets (Connection)

import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomsRepo (..), RoomId, RoomsMap)
import IntMapRepo (IntMapRepo)
import qualified IntMapRepo
import Utils.Logger (LgSeverity (LgInfo, LgMessage), logger)
import qualified Network.WebSockets as WS
import Server.Messages
import GameLogic.GameLogic (GameType(..))
import Server.MessageProcessor
import Server.Types
import Users.User (UserId)



class (GameRoomsRepo mvar) => WebSocketServer (mvar :: Type -> Type) where
  addConnection :: mvar WebSocketServerState -> WS.Connection -> TQueue WSMsgFormat -> IO ConnectionId
  removeConnection :: mvar WebSocketServerState -> ConnectionId -> IO ()
  newContainerVar :: a -> IO (mvar a)

  runWebSocketServer :: String -> Int -> IO (mvar WebSocketServerState)
  runWebSocketServer host port = do
    (state :: mvar WebSocketServerState) <- newContainerVar newWebSocketServerState
    (gameRoomsMap :: mvar RoomsMap) <- createGameRoomsRepo
    WS.runServer host port $ webSocketServer gameRoomsMap state
    pure state

  webSocketServer :: mvar RoomsMap -> mvar WebSocketServerState -> WS.ServerApp
  webSocketServer gameRoomsMap wSState = \pending -> do
    conn <- WS.acceptRequest pending
    checkForExistingUser conn -- TODO check for user
    connStateMessageQueue <- newTQueueIO :: IO (TQueue WSMsgFormat)
    idConn <- addConnection wSState conn connStateMessageQueue
    logger LgInfo $ show idConn ++ " connected"
    WS.withPingThread conn 30 (pure ()) $ do
      finally
        (runRWST (wsThreadMessageListner idConn) (ConnThreadReader conn gameRoomsMap) (ConnThreadState Nothing) >> pure ())
        (disconnect idConn)
    where
      disconnect idConn = do
        removeConnection wSState idConn
        logger LgInfo $ show idConn ++ " disconnected"

newWebSocketServerState :: WebSocketServerState
newWebSocketServerState = undefined

-- addConnToState :: WS.Connection -> TQueue WSMsgFormat -> WebSocketServerState -> (WebSocketServerState, ConnectionId)
-- addConnToState conn connStateMessageQueue wssState =
--   let repo = wsConnectionRepo wssState
--       connState = ConnectionState {connStateConnection = conn, connStateMessageQueue}
--       (newRepo, idConn) = IntMapRepo.append connState repo
--    in (WebSocketServerState {wsConnectionRepo = newRepo}, idConn)

-- removeConnFromState :: ConnectionId -> WebSocketServerState -> WebSocketServerState
-- removeConnFromState idConn wssState =
--   let repo = wsConnectionRepo wssState
--       newRepo = IntMapRepo.delete idConn repo
--    in WebSocketServerState {wsConnectionRepo = newRepo}

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

wsThreadMessageListner :: (GameRoomsRepo mvar) =>ConnectionId -> ConnThread mvar ()
wsThreadMessageListner idConn = forever $ do
  ConnThreadReader conn _ <- ask
  (msg :: WSMsgFormat) <- liftIO $ WS.receiveData conn
  liftIO $ logger LgInfo $ "RECIEVE (#" <> show idConn <> "): " <> Text.unpack msg
  case (toWebSocketInputMessage msg) of
    LogInOutMsg logMsg -> processMsgLogInOut logMsg
    InitJoinRoomMsg ijrMsg -> do
      userId <- liftIO $ userIdFromConnectionId idConn
      processInitJoinRoom userId ijrMsg
    GameActionMsg gameActMsg -> processGameActionMsg gameActMsg
    IncorrectMsg txts -> processIncorrectMsg txts

userIdFromConnectionId :: ConnectionId -> IO UserId
userIdFromConnectionId = undefined -- TODO implement


runWebSocketServerMVar :: String -> Int -> IO ()
runWebSocketServerMVar host port = (runWebSocketServer host port :: IO (MVar WebSocketServerState)) >> pure ()

runWebSocketServerTMVar :: String -> Int -> IO ()
runWebSocketServerTMVar host port = (runWebSocketServer host port :: IO (TMVar WebSocketServerState)) >> pure ()