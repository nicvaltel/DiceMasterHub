{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Server.Connection
  ( ConnectionsRepo (..),
    ConnectionStatus (..),
    ConnectionState (..),
    ConnectionId (ConnId),
  )
where

import qualified Network.WebSockets as WS
import Text.Printf (printf)
import Users.User

newtype ConnectionId = ConnId {unConnectionId :: Int}
  deriving (Show)

data ConnectionStatus = CSNormal | CSConnectionNotFound
  deriving (Show)

data ConnectionState = ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: AnyUserId, connStatus :: ConnectionStatus}

instance Show ConnectionState where
  show ConnectionState {connStateUserId, connStatus} =
    printf "ConnectionState{WS.Connection, connStateUserId = %s , connStatus = %s}" (show connStateUserId) (show connStatus)

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: ToAnyUserId r => db -> WS.Connection -> UserId r -> ConnectionStatus -> IO ConnectionId
  updateUser :: ToAnyUserId r => db -> ConnectionId -> UserId r -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe ConnectionState)
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe (UserId r))
  getConnStatus :: db -> ConnectionId -> IO ConnectionStatus
  nextAnonUserId :: db -> IO (UserId 'Anonim)
