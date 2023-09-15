{-# LANGUAGE InstanceSigs #-}

module GameRoom.GameRoom
  ( GameRoomsRepo (..),
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
import Users.User (UserId)

type RoomId = UserId

data GameRoomStatus = RoomWaitingForParticipant | GameInProgress | GameFinished GameResult

data GameRoomMessage

type RoomsMap = IntMap GameRoom

class GameRoomsRepo mvar where
  createGameRoomsRepo :: IO (mvar RoomsMap)
  createGameRoom :: UserId -> GameType -> mvar RoomsMap -> IO (Either RoomId RoomId)
  findUsersActiveRoom :: mvar RoomsMap -> IO (Maybe RoomId)

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

instance GameRoomsRepo TMVar where
  createGameRoomsRepo :: IO (TMVar RoomsMap)
  createGameRoomsRepo = newTMVarIO (IntMap.empty :: IntMap GameRoom)

  createGameRoom :: UserId -> GameType -> TMVar RoomsMap -> IO (Either RoomId RoomId)
  createGameRoom userId gameType mvRepo = do
    let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
    liftIO $ atomically $ do
      repo <- takeTMVar mvRepo
      case IntMap.lookup userId repo of
        Nothing -> do
          let newRepo = IntMap.insert userId newRoom repo
          putTMVar mvRepo newRepo
          pure (Right userId)
        Just _ -> pure (Left userId) -- gameroom for current user is already active

  findUsersActiveRoom :: TMVar RoomsMap -> IO (Maybe RoomId)
  findUsersActiveRoom mvRepo = do
    repo <- atomically $ readTMVar mvRepo
    pure Nothing

instance GameRoomsRepo MVar where
  createGameRoomsRepo :: IO (MVar RoomsMap)
  createGameRoomsRepo = newMVar IntMap.empty

  createGameRoom :: UserId -> GameType -> MVar RoomsMap -> IO (Either RoomId RoomId)
  createGameRoom userId gameType mvRepo = do
    let newRoom = newGameRoom userId gameType (newGameBoardState gameType)
    liftIO $ modifyMVarMasked mvRepo $ \repo ->
      case IntMap.lookup userId repo of
        Nothing ->
          let newRepo = IntMap.insert userId newRoom repo
           in pure (newRepo, Right userId)
        Just _ -> pure (repo, Left userId) -- gameroom for current user is already active
