{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module GameRoom.GameRoom
  ( GameRoomRepo (..),
    RoomId,
    GameRoomStatus (..),
    GameRoomMessage,
    NotStartedRoomsMap,
    InProgressRoomsMap,
    GameRoom (..),
    CreatedGameRoomId (..),
    newGameRoom,
  )
where

import Data.Map (Map)
import Data.Text (Text)
import GameLogic.GameLogic
  ( GameBoardState,
    GameMove,
    GameResult,
    GameType,
  )
import Types (Timestamp)
import Users.User

type RoomId = AnyUserId -- TODO chane RoomId from UserId to other

data GameRoomStatus = GameNotStarted | GameInProgress | GameFinished

data GameRoomMessage

type NotStartedRoomsMap = Map RoomId (GameRoom 'GameNotStarted)

type InProgressRoomsMap = Map RoomId (GameRoom 'GameInProgress)

data CreatedGameRoomId = NewCreatedRoom RoomId | AlreadyActiveRoom RoomId -- instead of Either
  deriving (Show)

-- data ActiveGameRoom = GameNotStartedRoom (GameRoom 'GameNotStarted) | GameInProgressRoom (GameRoom 'GameInProgress)

data GameRoom (u :: GameRoomStatus) = GameRoom
  { roomGameType :: GameType,
    roomStatus :: GameRoomStatus,
    roomUsers :: [AnyUserId],
    roomChat :: [(AnyUserId, Text)],
    roomGameActions :: [(AnyUserId, GameMove, Timestamp)],
    roomRoomActions :: [(AnyUserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState,
    gameResult :: Maybe GameResult
  }

newGameRoom :: AnyUserId -> GameType -> GameBoardState -> GameRoom 'GameNotStarted
newGameRoom anyUserId gameType gameBoardState =
  GameRoom
    { roomGameType = gameType,
      roomStatus = GameNotStarted,
      roomUsers = [anyUserId],
      roomChat = [],
      roomGameActions = [],
      roomRoomActions = [],
      roomBoardState = gameBoardState,
      gameResult = Nothing
    }

class GameRoomRepo db where
  createGameRoomRepo :: IO db
  createGameRoom :: db -> AnyUserId -> GameType -> IO CreatedGameRoomId
  findUsersActiveRoom :: db -> IO [AnyUserId]