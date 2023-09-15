{-# LANGUAGE InstanceSigs #-}
module GameRoom where

import IntMapRepo (IntMapRepo, empty, append)
import Types
import Data.Text (Text)
import Game
import User
import Control.Concurrent.STM (TMVar, newTMVarIO, atomically, putTMVar, takeTMVar)
import Control.Monad.Reader (ReaderT, MonadReader (ask), MonadIO (liftIO))

-- type Key = Int

type RoomId = Int

data GameRoomStatus = RoomWaitingForParticipant | GameInProgress | GameFinished GameResult

data GameRoomMessage

type RoomsMap = IntMapRepo GameRoom

type GameRoomRepoApp mvar a = ReaderT (mvar RoomsMap) IO a

class GameRoomsRepo mvar where
    createGameRoomsRepo :: IO (mvar RoomsMap)
    createGameRoom :: UserId -> GameType -> GameRoomRepoApp mvar RoomId

type GameRoomId = Int

data GameRoom = GameRoom {
    roomGameType :: GameType,
    roomStatus :: GameRoomStatus,
    roomUsers :: [UserId],
    roomChat :: [(UserId, Text)],
    roomGameActions :: [(UserId, GameMove, Timestamp)],
    roomRoomActions :: [(UserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState
}


newGameRoom :: UserId -> GameType -> GameBoardState -> GameRoom
newGameRoom userId gameType gameBoardState = GameRoom {
    roomGameType = gameType,
    roomStatus = RoomWaitingForParticipant,
    roomUsers = [userId],
    roomChat = [],
    roomGameActions = [],
    roomRoomActions = [],
    roomBoardState = gameBoardState
}

instance GameRoomsRepo TMVar where
    createGameRoomsRepo :: IO (TMVar RoomsMap)
    createGameRoomsRepo = newTMVarIO IntMapRepo.empty

    createGameRoom :: UserId -> GameType -> ReaderT (TMVar RoomsMap) IO GameRoomId
    createGameRoom userId gameType = do 
        let newRoom = newGameRoom userId gameType (newGameBoardState gameType) 
        mvRepo <- ask
        roomId <- liftIO $ atomically $ do
            repo <- takeTMVar mvRepo
            let (newRepo, roomId) = IntMapRepo.append newRoom repo
            putTMVar mvRepo newRepo
            pure roomId
        pure roomId

        