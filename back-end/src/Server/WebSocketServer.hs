-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.WebSocketServer (runWebSocketServer) where

import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomRepo (..))
import GameRoom.GameRoomTMVarAdapter
import qualified Network.WebSockets as WS
import Server.Connection
import Server.ConnectionTMVarAdapter
import Server.MessageProcessor
import Server.Messages
import Users.User (UserId(..))
import Utils.Utils (LgSeverity (LgInfo, LgError), logger)
import Users.UserPostgresAdapter (UserRepoDB)

type PingTime = Int

data WSSApp a = WSSApp {connRepo :: ConnectionRepoTMVar, gameRoomRepo :: GameRoomRepoTMVar, userRepo :: UserRepoDB}

class WebSocketServer wss where
  webSocketServer :: wss -> PingTime -> WS.ServerApp
  wsThreadMessageListener :: wss -> WS.Connection -> ConnectionId -> IO ()

instance WebSocketServer (WSSApp a) where
  webSocketServer :: WSSApp a -> PingTime -> WS.ServerApp
  webSocketServer wss@WSSApp{connRepo, gameRoomRepo, userRepo} pingTime = \pending -> do
    conn <- WS.acceptRequest pending
    userId <- checkForExistingUser conn
    idConn <- addConn connRepo conn userId CSNormal
    logger LgInfo $ show idConn ++ " connected"
    WS.withPingThread conn pingTime (pure ()) $ do
      finally
        (wsThreadMessageListener wss conn idConn)
        (disconnect idConn)
    where
      disconnect idConn = do
        removeConn connRepo idConn
        logger LgInfo $ show idConn ++ " disconnected"

  wsThreadMessageListener :: WSSApp a -> WS.Connection -> ConnectionId -> IO ()
  wsThreadMessageListener WSSApp{connRepo, gameRoomRepo, userRepo} conn idConn =
    forever $ do
      (msg :: WSMsgFormat) <- WS.receiveData conn
      logger LgInfo $ "RECIEVE #(" <> show idConn <> "): " <> Text.unpack msg
      connStatus <- getConnStatus connRepo idConn -- if connection status is not found, there will be CSConnectionNotFound
      case connStatus of -- FSM switcher
        CSNormal -> normalMessageProcessor msg
        CSConnectionNotFound -> logger LgError $ "ConnectionId not found, but message recieved idConn = " ++ show idConn   
      pure ()
    where
      normalMessageProcessor :: WSMsgFormat -> IO ()
      normalMessageProcessor msg = do 
        let wsMsg = toWebSocketInputMessage msg
        logger LgInfo (show wsMsg)
        case (toWebSocketInputMessage msg) of
          LogInOutMsg logMsg -> do
            mbUid <- processMsgLogInOut userRepo logMsg
            case mbUid of
              Just uId -> do
                updateUser connRepo idConn uId
              Nothing -> pure ()
          InitJoinRoomMsg ijrMsg -> do
            mbUserId <- userIdFromConnectionId connRepo idConn
            case mbUserId of
              Nothing -> undefined
              Just userId -> processInitJoinRoom userId ijrMsg
          GameActionMsg gameActMsg -> processGameActionMsg gameActMsg
          IncorrectMsg txts -> processIncorrectMsg txts

checkForExistingUser :: WS.Connection -> IO UserId
checkForExistingUser conn = pure (UserId 666) -- TODO implement


runWebSocketServer :: String -> Int -> PingTime -> UserRepoDB -> IO ()
runWebSocketServer host port pingTime userRepo = do
  gameRoomRepo :: GameRoomRepoTMVar <- createGameRoomRepo
  connRepo :: ConnectionRepoTMVar <- createConnsRepo
  let wss = WSSApp {connRepo, gameRoomRepo, userRepo}
  WS.runServer host port $ webSocketServer wss pingTime
