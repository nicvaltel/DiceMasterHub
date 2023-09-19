{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module GameRoom.GameRoom
  ( GameRoomRepo (..),
    RoomId,
    GameRoomStatus (..),
    GameRoomMessage,
    RoomsMap,
    GameRoom (..),
    newGameRoom,
  )
where

import Data.IntMap (IntMap)
import Data.Text (Text)
import GameLogic.GameLogic
  ( GameBoardState,
    GameMove,
    GameResult,
    GameType,
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