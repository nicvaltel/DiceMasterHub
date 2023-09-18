{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.MessageProcessor where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (ask)
import Data.Text (Text)
import qualified Data.Text as Text
import GameLogic.GameLogic (GameType (..))
import GameRoom.GameRoom
import qualified Network.WebSockets as WS
import Server.Messages
import Users.User (UserId(..), User (..), UserRepo (..))
import Utils.Utils
import Network.WebSockets (Connection)
import Users.UserPostgresAdapter (UserRepoDB(..))
import qualified PostgreSQLConnector as PG
import Database.PostgreSQL.Simple (query, Only (Only))




processMsgLogInOut :: UserRepoDB -> LogInOut -> IO (Maybe UserId)
processMsgLogInOut repo (Login username) = undefined
processMsgLogInOut repo Logout = undefined
processMsgLogInOut repo (Register username password) = do
  res <- addUser repo username password
  case res of
    usrId@(Just (UserId uId)) -> do 
      sendWebSocketOutputMessage $ RegisteredSuccessfullyMst uId
      pure usrId
    Nothing -> do 
      sendWebSocketOutputMessage RegisterErrorMsg
      pure Nothing


processInitJoinRoom ::  UserId -> InitJoinRoom -> IO ()
processInitJoinRoom userId (InitGameRoom params) = do
  pure ()
  -- ConnThreadReader _ mvarRooms <- ask
  -- mbNewRoomId <- liftIO $ createGameRoom userId (extractGameType params) mvarRooms -- TODO get GameType
  -- case mbNewRoomId of
  --   Right newRoomId -> sendWebSocketOutputMessage (GameRoomCreatedMsg newRoomId)
  --   Left oldRoomId -> sendWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg oldRoomId)
processInitJoinRoom userId (JoinGameRoom roomId) = undefined

processGameActionMsg :: GameAction -> IO ()
processGameActionMsg (GameAction params) = undefined

processIncorrectMsg :: [Text] -> IO ()
processIncorrectMsg _ = sendWebSocketOutputMessage ResendIncorrectMsg

sendWebSocketOutputMessage :: WebSocketOutputMessage -> IO ()
sendWebSocketOutputMessage msg = do
  -- liftIO $ logger LgMessage (Text.unpack $ fromWebSocketOutputMessage msg)
  -- ConnThreadReader conn _ <- ask
  -- liftIO $ WS.sendTextData conn (fromWebSocketOutputMessage msg :: Text)
  pure ()

extractGameType :: [WSMsgFormat] -> GameType
extractGameType = undefined -- TODO implement

askForExistingUser :: Connection -> IO ()
askForExistingUser conn = sendWebSocketOutputMessage AskForExistingUserMsg


