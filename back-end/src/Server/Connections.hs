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

type ConnectionId = Int

data ConnectionState = ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: UserId}

type ConnectionsMap = IntMapRepo ConnectionState -- key = ConnectionId

class ConnectionsRepo db where
  createConnsRepo :: IO (db)
  addConn :: db -> WS.Connection -> UserId -> IO ConnectionId
  updateUser :: db -> ConnectionId -> UserId -> IO ()
  removeConn :: db -> ConnectionId -> IO ()
  lookupConnState :: db -> ConnectionId -> IO (Maybe ConnectionState)
  userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe UserId)


instance ConnectionsRepo (TMVar ConnectionsMap) where
  createConnsRepo :: IO (TMVar ConnectionsMap)
  createConnsRepo = newTMVarIO IntMapRepo.empty

  addConn :: TMVar ConnectionsMap -> WS.Connection -> UserId -> IO ConnectionId
  addConn tmvRepo conn userId = atomically $ do
    repo <- takeTMVar tmvRepo
    let (newWssState, idConn) = addConnToState repo conn userId
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
  lookupConnState tmvRepo idConn = atomically $ (IntMapRepo.lookup idConn) <$> readTMVar tmvRepo

  userIdFromConnectionId :: TMVar ConnectionsMap -> ConnectionId -> IO (Maybe UserId)
  userIdFromConnectionId tmvRepo idConn = do
    mbConn <- lookupConnState tmvRepo idConn
    case mbConn of
      Nothing -> pure Nothing
      Just ConnectionState{connStateUserId} -> pure $ Just connStateUserId


addConnToState :: ConnectionsMap -> WS.Connection -> UserId -> (ConnectionsMap, ConnectionId)
addConnToState repo conn userId =
  let connState = ConnectionState {connStateConnection = conn, connStateUserId = userId}
      (newRepo, idConn) = IntMapRepo.append connState repo
   in (newRepo, idConn)

removeConnFromState :: ConnectionsMap -> ConnectionId -> ConnectionsMap
removeConnFromState repo idConn = IntMapRepo.delete idConn repo

updateUserInConnState :: ConnectionsMap -> ConnectionId -> UserId -> ConnectionsMap
updateUserInConnState repo connId userId = IntMapRepo.modify (\cs -> cs {connStateUserId = userId}) connId repo