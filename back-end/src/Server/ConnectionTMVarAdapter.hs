{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Users.User (UserId (..))

newtype ConnectionRepoTMVar = ConnectionRepoTMVar (TMVar ConnectionsMap)

data ConnectionsMap = ConnectionsMap
  { connsIntMap :: IntMapRepo ConnectionState, -- key = ConnectionId
    maxAnonUID :: UserId
  }

instance ConnectionsRepo ConnectionRepoTMVar where
  createConnsRepo :: IO ConnectionRepoTMVar
  createConnsRepo = do
    tmvRepo <- newTMVarIO ConnectionsMap {connsIntMap = IntMapRepo.empty, maxAnonUID = AnonUserId 0}
    pure (ConnectionRepoTMVar tmvRepo)

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
  lookupConnState (ConnectionRepoTMVar tmvRepo) (ConnId idConn) = do
    ConnectionsMap {connsIntMap} <- atomically $ readTMVar tmvRepo
    pure (IntMapRepo.lookup idConn connsIntMap)

  userIdFromConnectionId :: ConnectionRepoTMVar -> ConnectionId -> IO (Maybe UserId)
  userIdFromConnectionId connRepo idConn = (connStateUserId <$>) <$> lookupConnState connRepo idConn

  getConnStatus :: ConnectionRepoTMVar -> ConnectionId -> IO ConnectionStatus
  getConnStatus tmvRepo idConn = do
    mbConn <- lookupConnState tmvRepo idConn
    case mbConn of
      Nothing -> pure CSConnectionNotFound
      Just ConnectionState {connStatus} -> pure $ connStatus

  nextAnonUserId :: ConnectionRepoTMVar -> IO UserId
  nextAnonUserId (ConnectionRepoTMVar tmvRepo) = atomically $ do
    consMap@ConnectionsMap {maxAnonUID = maxUid} <- takeTMVar tmvRepo
    case maxUid of
      AnonUserId maxId -> do
        putTMVar tmvRepo consMap {maxAnonUID = AnonUserId (maxId + 1)}
        pure maxUid
      RegUserId _ -> error "Server.ConnectionTMVarAdapter nextAnonUserId: maxAnonUID contains non RegUserId"

addConnToState :: ConnectionsMap -> WS.Connection -> UserId -> ConnectionStatus -> (ConnectionsMap, ConnectionId)
addConnToState ConnectionsMap {connsIntMap, maxAnonUID} conn userId status =
  let connState = ConnectionState {connStateConnection = conn, connStateUserId = userId, connStatus = status}
      (newRepo, idConn) = IntMapRepo.append connState connsIntMap
   in (ConnectionsMap {connsIntMap = newRepo, maxAnonUID}, ConnId idConn)

removeConnFromState :: ConnectionsMap -> ConnectionId -> ConnectionsMap
removeConnFromState consMap (ConnId idConn) =
  consMap {connsIntMap = IntMapRepo.delete idConn (connsIntMap consMap)}

updateUserInConnState :: ConnectionsMap -> ConnectionId -> UserId -> ConnectionsMap
updateUserInConnState consMap@ConnectionsMap {connsIntMap} (ConnId connId) userId =
  consMap {connsIntMap = IntMapRepo.modify (\cs -> cs {connStateUserId = userId}) connId connsIntMap}
