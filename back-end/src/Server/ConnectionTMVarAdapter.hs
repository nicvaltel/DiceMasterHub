{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Server.ConnectionTMVarAdapter (ConnectionRepoTMVar(..)) where

import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newTMVarIO,
    putTMVar,
    readTMVar,
    takeTMVar,
  )
import qualified Network.WebSockets as WS
import Server.Connection
import Users.User (UserId (..), RegisteredUser(..))
import IntMapRepo
import Utils.Utils (LgSeverity(LgCritical), logger)
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (unsafeIOToSTM)

newtype ConnectionRepoTMVar = ConnectionRepoTMVar (TMVar ConnectionsMap)

data ConnectionsMap = ConnectionsMap {
  connsIntMap :: IntMapRepo ConnectionState, -- key = ConnectionId
  maxAnonUID :: UserId 'Anonim
  }

instance ConnectionsRepo ConnectionRepoTMVar where
  createConnsRepo :: IO ConnectionRepoTMVar
  createConnsRepo = do
    tmvRepo <- newTMVarIO ConnectionsMap{connsIntMap = IntMapRepo.empty, maxAnonUID = UserId 0}
    pure (ConnectionRepoTMVar tmvRepo)

  addConn :: ConnectionRepoTMVar -> WS.Connection -> UserId r -> ConnectionStatus -> IO ConnectionId
  addConn (ConnectionRepoTMVar tmvRepo) conn userId state = atomically $ do
    repo <- takeTMVar tmvRepo
    let (newWssState, idConn) = addConnToState repo conn userId state
    putTMVar tmvRepo newWssState
    pure idConn

  updateUser :: ConnectionRepoTMVar -> ConnectionId -> UserId r -> IO ()
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
  lookupConnState (ConnectionRepoTMVar tmvRepo) (ConnId idConn) = atomically $ (IntMapRepo.lookup idConn . connsIntMap) <$> readTMVar tmvRepo

  userIdFromConnectionId :: ConnectionRepoTMVar -> ConnectionId -> IO (Maybe (UserId r))
  userIdFromConnectionId connRepo idConn = (getConnStateUserId <$>) <$> lookupConnState connRepo idConn

  getConnStatus :: ConnectionRepoTMVar -> ConnectionId -> IO ConnectionStatus
  getConnStatus tmvRepo idConn = do
    mbConn <- lookupConnState tmvRepo idConn
    case mbConn of
      Nothing -> pure CSConnectionNotFound
      Just ConnectionState {connStatus} -> pure $ connStatus

  nextAnonUserId :: ConnectionRepoTMVar -> IO (UserId r)
  nextAnonUserId (ConnectionRepoTMVar tmvRepo) = atomically $ do
    consMap@ConnectionsMap{maxAnonUID = UserId maxId} <- takeTMVar tmvRepo
    putTMVar tmvRepo consMap{maxAnonUID = UserId (maxId + 1)}
    pure (UserId maxId)



addConnToState :: ConnectionsMap -> WS.Connection -> UserId r -> ConnectionStatus -> (ConnectionsMap, ConnectionId)
addConnToState ConnectionsMap{connsIntMap,maxAnonUID} conn userId status =
  let connState = ConnectionState {connStateConnection = conn, connStateUserId = userId, connStatus = status}
      (newRepo, idConn) = IntMapRepo.append connState connsIntMap
   in (ConnectionsMap{ connsIntMap = newRepo,maxAnonUID} , ConnId idConn)

removeConnFromState :: ConnectionsMap -> ConnectionId -> ConnectionsMap
removeConnFromState consMap (ConnId idConn) = 
  consMap{connsIntMap = IntMapRepo.delete idConn (connsIntMap consMap)}

updateUserInConnState :: ConnectionsMap -> ConnectionId -> UserId r -> ConnectionsMap
updateUserInConnState consMap (ConnId connId) userId = 
  undefined

  -- consMap{ connsIntMap = IntMapRepo.modify (\cs -> cs {connStateUserId = userId}) connId (connsIntMap consMap) }