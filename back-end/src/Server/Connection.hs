{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Server.Connection
  ( ConnectionsRepo (..),
    ConnectionStatus (..),
    ConnectionState (..),
    ConnectionId (ConnId),
    getConnStateUserId,
  )
where

import qualified Network.WebSockets as WS
import Users.User (UserId(..), RegStatus (..))
import Text.Printf (printf)
import Data.Data (typeOf, Typeable)

newtype ConnectionId = ConnId {unConnectionId :: Int}
  deriving (Show)

data ConnectionStatus = CSNormal | CSConnectionNotFound
  deriving (Show)

-- data ConnectionState = forall (r :: RegStatus). (Typeable r) => ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: UserId r, connStatus :: ConnectionStatus}

-- instance Show (ConnectionState ) where
--   show ConnectionState{connStateUserId, connStatus} = 
--     printf "ConnectionState{WS.Connection, connStateUserId = %s , connStatus = %s}" (show (typeOf connStateUserId) ++ show connStateUserId) (show connStatus)

data ConnectionState = forall (r :: RegStatus). ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: UserId r, connStatus :: ConnectionStatus}

instance Show ConnectionState where
  show ConnectionState{connStateUserId, connStatus} = 
    printf "ConnectionState{WS.Connection, connStateUserId = %s , connStatus = %s}" (show connStateUserId) (show connStatus)



getConnStateUserId :: ConnectionState -> UserId r
getConnStateUserId ConnectionState {connStateUserId = userId} = 
  let UserId uId = userId
  in UserId uId

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: db -> WS.Connection -> UserId r -> ConnectionStatus -> IO ConnectionId
  updateUser :: db -> ConnectionId -> UserId r -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe (ConnectionState ))
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe (UserId r))
  getConnStatus :: db -> ConnectionId -> IO ConnectionStatus
  nextAnonUserId :: db -> IO (UserId 'Anonim)
