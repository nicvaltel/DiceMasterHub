{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Connection
  ( ConnectionsRepo (..),
    ConnectionStatus (..),
    ConnectionsMap,
    ConnectionState (ConnectionState, connStatus, connStateUserId),
    ConnectionId (ConnId),
    addConnToState,
    removeConnFromState,
    updateUserInConnState,
  )
where

import IntMapRepo
import qualified Network.WebSockets as WS
import Users.User (UserId)

newtype ConnectionId = ConnId {unConnectionId :: Int}
  deriving (Show)

data ConnectionStatus = CSNormal | CSConnectionNotFound

data ConnectionState = ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: UserId, connStatus :: ConnectionStatus}

type ConnectionsMap = IntMapRepo ConnectionState -- key = ConnectionId

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: db -> WS.Connection -> UserId -> ConnectionStatus -> IO ConnectionId
  updateUser :: db -> ConnectionId -> UserId -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe ConnectionState)
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe UserId)
  getConnStatus :: db -> ConnectionId -> IO ConnectionStatus

addConnToState :: ConnectionsMap -> WS.Connection -> UserId -> ConnectionStatus -> (ConnectionsMap, ConnectionId)
addConnToState repo conn userId status =
  let connState = ConnectionState {connStateConnection = conn, connStateUserId = userId, connStatus = status}
      (newRepo, idConn) = IntMapRepo.append connState repo
   in (newRepo, ConnId idConn)

removeConnFromState :: ConnectionsMap -> ConnectionId -> ConnectionsMap
removeConnFromState repo (ConnId idConn) = IntMapRepo.delete idConn repo

updateUserInConnState :: ConnectionsMap -> ConnectionId -> UserId -> ConnectionsMap
updateUserInConnState repo (ConnId connId) userId = IntMapRepo.modify (\cs -> cs {connStateUserId = userId}) connId repo