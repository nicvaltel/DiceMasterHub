module GameRoom.GameRoom
  ( GameRoomRepo (..),
    RoomId,
    GameRoomStatus (..),
    GameRoomMessage,
    RoomsMap,
    GameRoom (..),
    CreatedGameRoom (..),
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
import Users.User (UserId (..))

type RoomId = UserId -- TODO chane RoomId from UserId to other

data GameRoomStatus = RoomWaitingForParticipant | GameInProgress | GameFinished GameResult

data GameRoomMessage

type RoomsMap = Map UserId GameRoom

data CreatedGameRoom = NewCreatedRoom RoomId | AlreadyActiveRoom RoomId
  deriving (Show)

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
  createGameRoom :: db -> UserId -> GameType -> IO CreatedGameRoom
  findUsersActiveRoom :: db -> IO (Maybe UserId)