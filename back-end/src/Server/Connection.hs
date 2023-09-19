{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Connection
  ( ConnectionsRepo (..),
    ConnectionStatus (..),
    ConnectionState (..),
    ConnectionId (ConnId),
  )
where

import qualified Network.WebSockets as WS
import Users.User (UserId)

newtype ConnectionId = ConnId {unConnectionId :: Int}
  deriving (Show)

data ConnectionStatus = CSNormal | CSConnectionNotFound

data ConnectionState = ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: UserId, connStatus :: ConnectionStatus}

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: db -> WS.Connection -> UserId -> ConnectionStatus -> IO ConnectionId
  updateUser :: db -> ConnectionId -> UserId -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe ConnectionState)
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe UserId)
  getConnStatus :: db -> ConnectionId -> IO ConnectionStatus
