-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunWebSocketServerMVar where

import Control.Concurrent (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar)
import Control.Concurrent.STM
  ( TQueue,
    atomically,
    newTQueueIO,
    writeTQueue,
  )
import Control.Exception (finally)
import Control.Monad (forever)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import Types (WebSocketMessage (WebSocketMessage))

data WebSocketServerState = WebSocketServerState
  { wsConnectionMap :: ConnectionMap
  }

data ConnectionState = ConnectionState
  { connStateConnection :: WS.Connection,
    connStateMessageQueue :: TQueue WebSocketMessage
  }

data ConnectionMap = ConnectionMap
  { freeConnIds :: [Int],
    maxId :: Int,
    connStateMap :: IntMap ConnectionState
  }

emptyConnectionMap :: ConnectionMap
emptyConnectionMap =
  ConnectionMap
    { freeConnIds = [],
      maxId = 0,
      connStateMap = IntMap.empty
    }

textToWebSocketMessage :: Text -> WebSocketMessage
textToWebSocketMessage txt = WebSocketMessage txt

textFromWebSocketMessage :: WebSocketMessage -> Text
textFromWebSocketMessage (WebSocketMessage txt) = txt

newWebSocketServerState :: WebSocketServerState
newWebSocketServerState =
  WebSocketServerState
    { wsConnectionMap = emptyConnectionMap
    }

main :: IO ()
main = do
  state <- newMVar newWebSocketServerState
  globalMessageQueue <- newTQueueIO
  WS.runServer "127.0.0.1" 1234 $ webSocketServer state globalMessageQueue

addConnToState :: WS.Connection -> WebSocketServerState -> IO (WebSocketServerState, Int)
addConnToState conn wssState = do
  let ConnectionMap {freeConnIds, maxId, connStateMap} = wsConnectionMap wssState
  let (idConn, freeConnIds', maxId') = case freeConnIds of
        [] -> (maxId, freeConnIds, maxId + 1)
        (headId : restIds) -> (headId, restIds, maxId)
  connStateMessageQueue <- newTQueueIO :: IO (TQueue WebSocketMessage)
  let connState = ConnectionState {connStateConnection = conn, connStateMessageQueue}
  let wsConnectionMap =
        ConnectionMap
          { freeConnIds = freeConnIds',
            maxId = maxId',
            connStateMap = IntMap.insert idConn connState connStateMap
          }
  pure (WebSocketServerState {wsConnectionMap}, idConn)

removeConnFromState :: Int -> WebSocketServerState -> IO WebSocketServerState
removeConnFromState idConn wssState = do
  let ConnectionMap {freeConnIds, maxId, connStateMap} = wsConnectionMap wssState
  let wsConnectionMap =
        ConnectionMap
          { freeConnIds = idConn : freeConnIds,
            maxId = maxId,
            connStateMap = IntMap.delete idConn connStateMap
          }
  pure WebSocketServerState {wsConnectionMap}

webSocketServer :: MVar WebSocketServerState -> TQueue WebSocketMessage -> WS.ServerApp
webSocketServer mvWSState globalMessageQueue = \pending -> do
  conn <- WS.acceptRequest pending
  checkForExistingUser conn -- TODO check for user
  idConn <- modifyMVarMasked mvWSState (addConnToState conn)
  logApp $ show idConn ++ " connected"
  WS.withPingThread conn 30 (pure ()) $ do
    flip finally (disconnect idConn) $ forever $ do
      (txt :: Text) <- WS.receiveData conn
      logApp $ "RECIEVE (#" <> show idConn <> "): " <> Text.unpack txt
      atomically $ writeTQueue globalMessageQueue (textToWebSocketMessage txt)
  where
    disconnect idConn = do
      modifyMVarMasked_ mvWSState (removeConnFromState idConn)
      logApp $ show idConn ++ " disconnected"

checkForExistingUser :: WS.Connection -> IO ()
checkForExistingUser _ = pure ()

logApp :: String -> IO ()
logApp = print
