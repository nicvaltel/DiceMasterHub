{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.WebSocketServerClass
  ( PingTime,
    WebSocketServer (..),
  )
where

import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomRepo (..))
import qualified Network.WebSockets as WS
import Server.Connection
import Server.Messages
import Users.User (UserId (..), UserRepo, RegStatus (..))
import Utils.Utils (LgSeverity (..), logger)

type PingTime = Int

class (ConnectionsRepo crepo, GameRoomRepo grrepo, UserRepo urepo) => WebSocketServer wss crepo grrepo urepo | wss -> crepo grrepo urepo where
  getConnRepo :: wss -> crepo
  getGameRoomRepo :: wss -> grrepo
  getUserRepo :: wss -> urepo
  webSocketServer :: wss -> PingTime -> WS.ServerApp
  wsThreadMessageListener :: wss -> WS.Connection -> ConnectionId -> IO ()
  checkForExistingUser :: wss -> WS.Connection -> IO (UserId 'Anonim)