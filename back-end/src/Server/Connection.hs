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
    case connStateUserId of 
      Left uId -> printf "ConnectionState{WS.Connection, connStateUserId = Anon %s , connStatus = %s}" (show uId) (show connStatus)
      Right uId -> printf "ConnectionState{WS.Connection, connStateUserId = Registered %s , connStatus = %s}" (show uId) (show connStatus)

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: ToAnyUserId r => db -> WS.Connection -> UserId r -> ConnectionStatus -> IO ConnectionId
  updateUserId :: ToAnyUserId r => db -> ConnectionId -> UserId r -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe ConnectionState)
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe (UserId r))
  getConnStatus :: db -> ConnectionId -> IO ConnectionStatus
  nextAnonUserId :: db -> IO (UserId 'Anonim)
