{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module GameRoom.GameRoom
  ( GameRoomRepo (..),
    RoomId,
    GameRoomStatus (..),
    GameRoomMessage,
    RoomsMap,
  )
where

import Control.Concurrent (MVar, modifyMVarMasked, newMVar)
import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, putTMVar, readTMVar, takeTMVar)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import GameLogic.GameLogic
  ( GameBoardState,
    GameMove,
    GameResult,
    GameType,
    newGameBoardState,
  )
import Types (Timestamp)
import Users.User (UserId (..))

type RoomId = Int -- UserId

data GameRoomStatus = RoomWaitingForParticipant | GameInProgress | GameFinished GameResult

data GameRoomMessage

type RoomsMap = IntMap GameRoom



data GameRoom = GameRoom
  { roomGameType :: GameType,
    roomStatus :: GameRoomStatus,
    roomUsers :: [UserId],
    roomChat :: [(UserId, Text)],
    roomGameActions :: [(UserId, GameMove, Timestamp)],
    roomRoomActions :: [(UserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState
  }

newGameRoom :: UserId -> GameType -> GameBoardState -> GameRoom
newGameRoom userId gameType gameBoardState =
  GameRoom
    { roomGameType = gameType,
      roomStatus = RoomWaitingForParticipant,
      roomUsers = [userId],
      roomChat = [],
      roomGameActions = [],
      roomRoomActions = [],
      roomBoardState = gameBoardState
    }

class GameRoomRepo db where
  createGameRoomRepo :: IO db
  createGameRoom :: db -> UserId -> GameType -> IO (Either UserId UserId) -- if Left then -- gameroom for current user is already active
  findUsersActiveRoom :: db -> IO (Maybe UserId)

instance GameRoomRepo (TMVar RoomsMap) where
  createGameRoomRepo :: IO (TMVar RoomsMap)
  createGameRoomRepo = newTMVarIO (IntMap.empty :: IntMap GameRoom)

  createGameRoom :: TMVar RoomsMap -> UserId -> GameType ->  IO (Either UserId UserId)
  createGameRoom mvRepo userId gameType  = do
    let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
    liftIO $ atomically $ do
      repo <- takeTMVar mvRepo
      case IntMap.lookup (unUserId userId) repo of
        Nothing -> do
          let newRepo = IntMap.insert (unUserId userId) newRoom repo
          putTMVar mvRepo newRepo
          pure (Right userId)
        Just _ -> pure (Left userId) -- gameroom for current user is already active

  -- findUsersActiveRoom :: TMVar RoomsMap -> IO (Maybe RoomId)
  -- findUsersActiveRoom mvRepo = do
  --   repo <- atomically $ readTMVar mvRepo
  --   pure Nothing

instance GameRoomRepo (MVar RoomsMap) where
  createGameRoomRepo :: IO (MVar RoomsMap)
  createGameRoomRepo = newMVar IntMap.empty

  createGameRoom :: MVar RoomsMap -> UserId -> GameType ->  IO (Either UserId UserId)
  createGameRoom mvRepo userId gameType  = do
    let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
    liftIO $ modifyMVarMasked mvRepo $ \repo ->
      case IntMap.lookup (unUserId userId) repo of
        Nothing ->
          let newRepo = IntMap.insert (unUserId userId) newRoom repo
           in pure (newRepo, Right userId)
        Just _ -> pure (repo, Left userId) -- gameroom for current user is already active
