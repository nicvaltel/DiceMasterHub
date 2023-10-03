{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Server.ConnectionTMVarAdapter (ConnectionRepoTMVar (..)) where

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
import Server.Connection
import Users.User 


newtype ConnectionRepoTMVar = ConnectionRepoTMVar (TMVar ConnectionsMap)

data ConnectionsMap = ConnectionsMap
  { connsIntMap :: IntMapRepo ConnectionState, -- key = ConnectionId
    maxAnonUID :: UserId 'Anonim
  }

instance ConnectionsRepo ConnectionRepoTMVar where
  createConnsRepo :: IO ConnectionRepoTMVar
  createConnsRepo = do
    tmvRepo <- newTMVarIO ConnectionsMap {connsIntMap = IntMapRepo.empty, maxAnonUID = UserId 0}
    pure (ConnectionRepoTMVar tmvRepo)

  addConn :: ToAnyUserId r => ConnectionRepoTMVar -> WS.Connection -> UserId r -> ConnectionStatus -> IO ConnectionId
  addConn (ConnectionRepoTMVar tmvRepo) conn userId state = atomically $ do
    repo <- takeTMVar tmvRepo
    let (newWssState, idConn) = addConnToState repo conn userId state
    putTMVar tmvRepo newWssState
    pure idConn

  updateUserId :: ToAnyUserId r => ConnectionRepoTMVar -> ConnectionId -> UserId r -> IO ()
  updateUserId (ConnectionRepoTMVar tmvRepo) connId userId = atomically $ do
    repo <- takeTMVar tmvRepo
    let newRepo = updateUserInConnState repo connId userId
    putTMVar tmvRepo newRepo

  removeConn :: ConnectionRepoTMVar -> ConnectionId -> IO ()
  removeConn (ConnectionRepoTMVar tmvRepo) idConn = atomically $ do
    repo <- takeTMVar tmvRepo
    let newWssState = removeConnFromState repo idConn
    putTMVar tmvRepo newWssState

  lookupConnState :: ConnectionRepoTMVar -> ConnectionId -> IO (Maybe ConnectionState)
  lookupConnState (ConnectionRepoTMVar tmvRepo) (ConnId idConn) = do
    ConnectionsMap {connsIntMap} <- atomically $ readTMVar tmvRepo
    pure (IntMapRepo.lookup idConn connsIntMap)

  userIdFromConnectionId :: ConnectionRepoTMVar -> ConnectionId -> IO (Maybe (UserId r))
  userIdFromConnectionId connRepo idConn = (fromAnyUserId . connStateUserId <$>) <$> lookupConnState connRepo idConn

  getConnStatus :: ConnectionRepoTMVar -> ConnectionId -> IO ConnectionStatus
  getConnStatus tmvRepo idConn = do
    mbConn <- lookupConnState tmvRepo idConn
    case mbConn of
      Nothing -> pure CSConnectionNotFound
      Just ConnectionState {connStatus} -> pure $ connStatus

  nextAnonUserId :: ConnectionRepoTMVar -> IO (UserId 'Anonim)
  nextAnonUserId (ConnectionRepoTMVar tmvRepo) = atomically $ do
    consMap@ConnectionsMap {maxAnonUID = anonUId@(UserId maxUid)} <- takeTMVar tmvRepo
    putTMVar tmvRepo consMap {maxAnonUID = UserId (maxUid + 1)}
    pure anonUId

addConnToState :: ToAnyUserId r => ConnectionsMap -> WS.Connection -> UserId r -> ConnectionStatus -> (ConnectionsMap, ConnectionId)
addConnToState ConnectionsMap {connsIntMap, maxAnonUID} conn userId status = 
  let connState = ConnectionState {connStateConnection = conn, connStateUserId = toAnyUserId userId, connStatus = status}
      (newRepo, idConn) = IntMapRepo.append connState connsIntMap
   in (ConnectionsMap {connsIntMap = newRepo, maxAnonUID}, ConnId idConn)

removeConnFromState :: ConnectionsMap -> ConnectionId -> ConnectionsMap
removeConnFromState consMap (ConnId idConn) =
  consMap {connsIntMap = IntMapRepo.delete idConn (connsIntMap consMap)}

updateUserInConnState :: ToAnyUserId r => ConnectionsMap -> ConnectionId -> UserId r -> ConnectionsMap
updateUserInConnState consMap@ConnectionsMap {connsIntMap} (ConnId connId) userId = 
  consMap {connsIntMap = IntMapRepo.modify (\cs -> cs {connStateUserId = toAnyUserId userId}) connId connsIntMap}
