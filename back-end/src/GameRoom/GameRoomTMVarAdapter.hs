{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module GameRoom.GameRoomTMVarAdapter
  ( GameRoomRepoTMVar (..),
  )
where

import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, putTMVar, takeTMVar)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import GameLogic.GameLogic
  ( GameType,
    newGameBoardState,
  )
import GameRoom.GameRoom
import Users.User (UserId (..))

newtype GameRoomRepoTMVar = GameRoomRepoTMVar (TMVar RoomsMap)

instance GameRoomRepo GameRoomRepoTMVar where
  createGameRoomRepo :: IO GameRoomRepoTMVar
  createGameRoomRepo = GameRoomRepoTMVar <$> newTMVarIO (Map.empty :: Map RoomId GameRoom)

  createGameRoom :: GameRoomRepoTMVar -> UserId -> GameType -> IO CreatedGameRoom
  createGameRoom (GameRoomRepoTMVar tmvRepo) userId gameType = do
    let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
    atomically $ do
      repo <- takeTMVar tmvRepo
      case Map.lookup userId repo of
        Nothing -> do
          let newRepo = Map.insert userId newRoom repo
          putTMVar tmvRepo newRepo
          pure (NewCreatedRoom userId)
        Just _ -> pure (AlreadyActiveRoom userId) -- gameroom for current user is already active
