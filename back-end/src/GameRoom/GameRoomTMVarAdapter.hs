{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module GameRoom.GameRoomTMVarAdapter
  ( GameRoomRepoTMVar (..),
  )
where

import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, putTMVar, readTMVar, takeTMVar)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GameLogic.GameLogic
  ( GameType,
    newGameBoardState,
  )
import GameRoom.GameRoom
import Users.User (UserId (..))

newtype GameRoomRepoTMVar = GameRoomRepoTMVar (TMVar RoomsMap)

instance GameRoomRepo GameRoomRepoTMVar where
  createGameRoomRepo :: IO GameRoomRepoTMVar
  createGameRoomRepo = GameRoomRepoTMVar <$> newTMVarIO (IntMap.empty :: IntMap GameRoom)

  createGameRoom :: GameRoomRepoTMVar -> UserId -> GameType -> IO (Either UserId UserId)
  createGameRoom (GameRoomRepoTMVar tmvRepo) userId gameType = do
    let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
    liftIO $ atomically $ do
      repo <- takeTMVar tmvRepo
      case IntMap.lookup (unUserId userId) repo of
        Nothing -> do
          let newRepo = IntMap.insert (unUserId userId) newRoom repo
          putTMVar tmvRepo newRepo
          pure (Right userId)
        Just _ -> pure (Left userId) -- gameroom for current user is already active

-- instance GameRoomRepo (MVar RoomsMap) where
--   createGameRoomRepo :: IO (MVar RoomsMap)
--   createGameRoomRepo = newMVar IntMap.empty

--   createGameRoom :: MVar RoomsMap -> UserId -> GameType ->  IO (Either UserId UserId)
--   createGameRoom mvRepo userId gameType  = do
--     let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
--     liftIO $ modifyMVarMasked mvRepo $ \repo ->
--       case IntMap.lookup (unUserId userId) repo of
--         Nothing ->
--           let newRepo = IntMap.insert (unUserId userId) newRoom repo
--            in pure (newRepo, Right userId)
--         Just _ -> pure (repo, Left userId) -- gameroom for current user is already active
