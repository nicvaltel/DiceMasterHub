{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

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
import Users.User (UserId (..), RegisteredUser)

type RoomId = Int -- UserId

data GameRoomStatus = RoomWaitingForParticipant | GameInProgress | GameFinished GameResult

data GameRoomMessage

type RoomsMap = IntMap GameRoom

data GameRoom = forall (r :: RegisteredUser). GameRoom
  { roomGameType :: GameType,
    roomStatus :: GameRoomStatus,
    roomUsers :: [UserId r],
    roomChat :: [(UserId r, Text)],
    roomGameActions :: [(UserId r, GameMove, Timestamp)],
    roomRoomActions :: [(UserId r, GameMove, Timestamp)],
    roomBoardState :: GameBoardState
  }

newGameRoom :: UserId r -> GameType -> GameBoardState -> GameRoom
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
  createGameRoom :: db -> UserId r -> GameType -> IO (Either (UserId r) (UserId r)) -- if Left then -- gameroom for current user is already active
  findUsersActiveRoom :: db -> IO (Maybe (UserId r))