{-# LANGUAGE NamedFieldPuns #-}

module Server.Connection
  ( ConnectionsRepo (..),
    ConnectionStatus (..),
    ConnectionState (..),
    ConnectionId (ConnId),
  )
where

import qualified Network.WebSockets as WS
import Text.Printf (printf)
import Users.User (UserId (..), AnonUId)

newtype ConnectionId = ConnId {unConnectionId :: Int}
  deriving (Show)

data ConnectionStatus = CSNormal | CSConnectionNotFound
  deriving (Show)

data ConnectionState = ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: UserId, connStatus :: ConnectionStatus}

instance Show ConnectionState where
  show ConnectionState {connStateUserId, connStatus} =
    printf "ConnectionState{WS.Connection, connStateUserId = %s , connStatus = %s}" (show connStateUserId) (show connStatus)

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: db -> WS.Connection -> UserId -> ConnectionStatus -> IO ConnectionId
  updateUser :: db -> ConnectionId -> UserId -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe ConnectionState)
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe UserId)
  getConnStatus :: db -> ConnectionId -> IO ConnectionStatus
  nextAnonUserId :: db -> IO AnonUId
