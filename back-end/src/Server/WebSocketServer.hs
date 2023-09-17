-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.WebSocketServer (runWebSocketServer) where

import Control.Concurrent.STM
  ( TMVar,
  )
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomRepo (..), RoomsMap)
import qualified Network.WebSockets as WS
import Server.Connections
import Server.MessageProcessor
import Server.Messages
import Users.User (UserId(..))
import Utils.Utils (LgSeverity (LgInfo, LgError), logger, tshow)

type PingTime = Int

data WSSApp a = WSSApp {wssConnRepo :: TMVar ConnectionsMap, wssGameRoomRepo :: TMVar RoomsMap, wssUserRepo :: IO a}

class WebSocketServer wss where
  webSocketServer :: wss -> PingTime -> WS.ServerApp
  wsThreadMessageListener :: wss -> WS.Connection -> ConnectionId -> IO ()

instance WebSocketServer (WSSApp a) where
  webSocketServer :: WSSApp a -> PingTime -> WS.ServerApp
  webSocketServer wss@WSSApp{wssConnRepo, wssGameRoomRepo, wssUserRepo} pingTime = \pending -> do
    conn <- WS.acceptRequest pending
    userId <- checkForExistingUser conn
    idConn <- addConn wssConnRepo conn userId CSNormal
    logger LgInfo $ show idConn ++ " connected"
    WS.withPingThread conn pingTime (pure ()) $ do
      finally
        (wsThreadMessageListener wss conn idConn)
        (disconnect idConn)
    where
      disconnect idConn = do
        removeConn wssConnRepo idConn
        logger LgInfo $ show idConn ++ " disconnected"

  wsThreadMessageListener :: WSSApp a -> WS.Connection -> ConnectionId -> IO ()
  wsThreadMessageListener WSSApp{wssConnRepo, wssGameRoomRepo, wssUserRepo} conn idConn =
    forever $ do
      (msg :: WSMsgFormat) <- WS.receiveData conn
      logger LgInfo $ "RECIEVE #(" <> show idConn <> "): " <> Text.unpack msg
      connStatus <- getConnStatus wssConnRepo idConn -- if connection status is not found, there will be CSConnectionNotFound
      case connStatus of -- FSM switcher
        CSNormal -> normalMessageProcessor msg
        CSConnectionNotFound -> logger LgError $ "ConnectionId not found, but message recieved idConn = " ++ show idConn   
      pure ()
    where
      normalMessageProcessor :: WSMsgFormat -> IO ()
      normalMessageProcessor msg = 
          case (toWebSocketInputMessage msg) of
          LogInOutMsg logMsg -> processMsgLogInOut logMsg
          InitJoinRoomMsg ijrMsg -> do
            mbUserId <- userIdFromConnectionId wssConnRepo idConn
            case mbUserId of
              Nothing -> undefined
              Just userId -> processInitJoinRoom userId ijrMsg
          GameActionMsg gameActMsg -> processGameActionMsg gameActMsg
          IncorrectMsg txts -> processIncorrectMsg txts

checkForExistingUser :: WS.Connection -> IO UserId
checkForExistingUser conn = pure (UserId 666) -- TODO implement


runWebSocketServer :: String -> Int -> PingTime -> IO ()
runWebSocketServer host port pingTime = do
  grRepo :: TMVar RoomsMap <- createGameRoomRepo
  cnRepo :: TMVar ConnectionsMap <- createConnsRepo
  let wss = WSSApp {wssConnRepo = cnRepo, wssGameRoomRepo = grRepo, wssUserRepo = undefined}
  WS.runServer host port $ webSocketServer wss pingTime
