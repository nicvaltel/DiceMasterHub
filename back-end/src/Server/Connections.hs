{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Connections
  ( ConnectionId,
    ConnectionState,
    ConnectionsRepo (..),
    ConnectionsMap,
    ConnectionStatus(..)
  )
where

import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newTMVarIO,
    putTMVar,
    readTMVar,
    takeTMVar,
  )
import IntMapRepo
import qualified Network.WebSockets as WS
import Users.User (UserId)

newtype ConnectionId = ConnId { unConnectionId :: Int }
  deriving Show

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


instance ConnectionsRepo (TMVar ConnectionsMap) where
  createConnsRepo :: IO (TMVar ConnectionsMap)
  createConnsRepo = newTMVarIO IntMapRepo.empty

  addConn :: TMVar ConnectionsMap -> WS.Connection -> UserId -> ConnectionStatus -> IO ConnectionId
  addConn tmvRepo conn userId state = atomically $ do
    repo <- takeTMVar tmvRepo
    let (newWssState, idConn) = addConnToState repo conn userId state
    putTMVar tmvRepo newWssState
    pure idConn

  updateUser :: TMVar ConnectionsMap -> ConnectionId -> UserId -> IO ()
  updateUser tmvRepo connId userId = atomically $ do
    repo <- takeTMVar tmvRepo
    let newRepo = updateUserInConnState repo connId userId
    putTMVar tmvRepo newRepo

  removeConn :: TMVar ConnectionsMap -> ConnectionId -> IO ()
  removeConn tmvRepo idConn = atomically $ do
    repo <- takeTMVar tmvRepo
    let newWssState = removeConnFromState repo idConn
    putTMVar tmvRepo newWssState

  lookupConnState :: TMVar ConnectionsMap -> ConnectionId -> IO (Maybe ConnectionState)
  lookupConnState tmvRepo (ConnId idConn) = atomically $ (IntMapRepo.lookup idConn) <$> readTMVar tmvRepo

  userIdFromConnectionId :: TMVar ConnectionsMap -> ConnectionId -> IO (Maybe UserId)
  userIdFromConnectionId tmvRepo idConn = (connStateUserId <$>) <$> lookupConnState tmvRepo idConn

  getConnStatus ::  TMVar ConnectionsMap -> ConnectionId -> IO ConnectionStatus
  getConnStatus tmvRepo idConn = do
    mbConn <- lookupConnState tmvRepo idConn
    case mbConn of
      Nothing -> pure CSConnectionNotFound
      Just ConnectionState{connStatus} -> pure $ connStatus


addConnToState :: ConnectionsMap -> WS.Connection -> UserId -> ConnectionStatus -> (ConnectionsMap, ConnectionId)
addConnToState repo conn userId status =
  let connState = ConnectionState {connStateConnection = conn, connStateUserId = userId, connStatus = status}
      (newRepo, idConn) = IntMapRepo.append connState repo
   in (newRepo, ConnId idConn)

removeConnFromState :: ConnectionsMap -> ConnectionId -> ConnectionsMap
removeConnFromState repo (ConnId idConn) = IntMapRepo.delete idConn repo

updateUserInConnState :: ConnectionsMap -> ConnectionId -> UserId -> ConnectionsMap
updateUserInConnState repo (ConnId connId) userId = IntMapRepo.modify (\cs -> cs {connStateUserId = userId}) connId repo