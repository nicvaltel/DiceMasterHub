{-# LANGUAGE DataKinds #-}
-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Server.WebSocketServerImpl  where

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
import Server.WebSocketServerClass
import Unsafe.Coerce (unsafeCoerce)
import Users.User (UserId (..), UserRepo, RegStatus (..))
import Users.UserPostgresAdapter (UserRepoDB)
import Utils.Utils (LgSeverity (LgError, LgInfo), logger)
import qualified Server.WebSocketServer as WSS


data WSSApp a = WSSApp {connRepo :: ConnectionRepoTMVar, gameRoomRepo :: GameRoomRepoTMVar, userRepo :: UserRepoDB}

instance WebSocketServer (WSSApp a) ConnectionRepoTMVar GameRoomRepoTMVar UserRepoDB where
  getConnRepo :: WSSApp a -> crepo
  getConnRepo = unsafeCoerce . connRepo

  getGameRoomRepo :: WSSApp a -> grrepo
  getGameRoomRepo = unsafeCoerce . gameRoomRepo

  getUserRepo :: WSSApp a -> crepo
  getUserRepo = unsafeCoerce . userRepo

  webSocketServer :: WSSApp a -> PingTime -> WS.ServerApp
  webSocketServer = WSS.webSocketServer'

  wsThreadMessageListener :: WSSApp a -> WS.Connection -> ConnectionId -> IO ()
  wsThreadMessageListener = WSS.wsThreadMessageListener'

  checkForExistingUser :: WSSApp a -> WS.Connection -> IO (UserId 'Anonim)
  checkForExistingUser = WSS.checkForExistingUser'

runWebSocketServer :: String -> Int -> PingTime -> UserRepoDB -> IO ()
runWebSocketServer host port pingTime userRepo = do
  gameRoomRepo :: GameRoomRepoTMVar <- createGameRoomRepo
  connRepo :: ConnectionRepoTMVar <- createConnsRepo
  let wss = WSSApp {connRepo, gameRoomRepo, userRepo}
  WS.runServer host port $ webSocketServer wss pingTime
