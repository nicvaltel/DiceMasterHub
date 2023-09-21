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
import Users.User

type RoomId = AnyUserId -- TODO chane RoomId from UserId to other

data GameRoomStatus = RoomWaitingForParticipant | GameInProgress | GameFinished GameResult

data GameRoomMessage

type RoomsMap = Map AnyUserId GameRoom

data CreatedGameRoom = NewCreatedRoom AnyUserId | AlreadyActiveRoom AnyUserId
  deriving (Show)

data GameRoom = GameRoom
  { roomGameType :: GameType,
    roomStatus :: GameRoomStatus,
    roomUsers :: [AnyUserId],
    roomChat :: [(AnyUserId, Text)],
    roomGameActions :: [(AnyUserId, GameMove, Timestamp)],
    roomRoomActions :: [(AnyUserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState
  }

newGameRoom :: AnyUserId -> GameType -> GameBoardState -> GameRoom
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
  createGameRoom :: db -> AnyUserId -> GameType -> IO CreatedGameRoom
  findUsersActiveRoom :: db -> IO (Maybe AnyUserId)