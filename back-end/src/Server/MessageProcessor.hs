{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Server.MessageProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import GameLogic.GameLogic (GameType (..))
import qualified Network.WebSockets as WS
import Server.Messages
import Users.User (UserId(..), UserRepo (..), RegisteredUser (Registered))
import Utils.Utils
import Network.WebSockets (Connection)




processMsgLogInOut :: UserRepo urepo => urepo -> LogInOut -> IO (Maybe (UserId 'Registered))
processMsgLogInOut repo (Login username) = undefined
processMsgLogInOut repo Logout = undefined
processMsgLogInOut repo (Register username password) = do
  res <- addUser repo username password
  case res of
    usrId@(Just (UserId uId)) -> do 
      sendWebSocketOutputMessage undefined $ RegisteredSuccessfullyMst uId
      pure usrId
    Nothing -> do 
      sendWebSocketOutputMessage undefined RegisterErrorMsg
      pure Nothing


processInitJoinRoom ::  UserId r -> InitJoinRoom -> IO ()
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
processIncorrectMsg _ = sendWebSocketOutputMessage undefined ResendIncorrectMsg

processUpdateExistingUser = undefined
-- processUpdateExistingUser :: wss -> ConnectionId -> Int -> IO ()
-- processUpdateExistingUser wss connId userId = pure ()

sendWebSocketOutputMessage :: Connection -> WebSocketOutputMessage -> IO ()
sendWebSocketOutputMessage conn msg = do
  logger LgMessage (Text.unpack $ fromWebSocketOutputMessage msg)
  WS.sendTextData conn (fromWebSocketOutputMessage msg :: Text)

extractGameType :: [WSMsgFormat] -> GameType
extractGameType = undefined -- TODO implement

askForExistingUser :: Connection -> IO ()
askForExistingUser conn = sendWebSocketOutputMessage conn AskForExistingUserMsg


