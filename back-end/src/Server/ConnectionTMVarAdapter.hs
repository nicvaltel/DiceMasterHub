{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.ConnectionTMVarAdapter (ConnectionRepoTMVar) where

import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newTMVarIO,
    putTMVar,
    readTMVar,
    takeTMVar,
  )
import IntMapRepo (empty, lookup)
import qualified Network.WebSockets as WS
import Server.Connection
import Users.User (UserId)

newtype ConnectionRepoTMVar = ConnectionRepoTMVar (TMVar ConnectionsMap)

instance ConnectionsRepo ConnectionRepoTMVar where
  createConnsRepo :: IO ConnectionRepoTMVar
  createConnsRepo = ConnectionRepoTMVar <$> newTMVarIO IntMapRepo.empty

  addConn :: ConnectionRepoTMVar -> WS.Connection -> UserId -> ConnectionStatus -> IO ConnectionId
  addConn (ConnectionRepoTMVar tmvRepo) conn userId state = atomically $ do
    repo <- takeTMVar tmvRepo
    let (newWssState, idConn) = addConnToState repo conn userId state
    putTMVar tmvRepo newWssState
    pure idConn

  updateUser :: ConnectionRepoTMVar -> ConnectionId -> UserId -> IO ()
  updateUser (ConnectionRepoTMVar tmvRepo) connId userId = atomically $ do
    repo <- takeTMVar tmvRepo
    let newRepo = updateUserInConnState repo connId userId
    putTMVar tmvRepo newRepo

  removeConn :: ConnectionRepoTMVar -> ConnectionId -> IO ()
  removeConn (ConnectionRepoTMVar tmvRepo) idConn = atomically $ do
    repo <- takeTMVar tmvRepo
    let newWssState = removeConnFromState repo idConn
    putTMVar tmvRepo newWssState

  lookupConnState :: ConnectionRepoTMVar -> ConnectionId -> IO (Maybe ConnectionState)
  lookupConnState (ConnectionRepoTMVar tmvRepo) (ConnId idConn) = atomically $ (IntMapRepo.lookup idConn) <$> readTMVar tmvRepo

  userIdFromConnectionId :: ConnectionRepoTMVar -> ConnectionId -> IO (Maybe UserId)
  userIdFromConnectionId tmvRepo idConn = (connStateUserId <$>) <$> lookupConnState tmvRepo idConn

  getConnStatus :: ConnectionRepoTMVar -> ConnectionId -> IO ConnectionStatus
  getConnStatus tmvRepo idConn = do
    mbConn <- lookupConnState tmvRepo idConn
    case mbConn of
      Nothing -> pure CSConnectionNotFound
      Just ConnectionState {connStatus} -> pure $ connStatus
