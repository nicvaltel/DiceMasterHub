{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.MessageProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import GameLogic.GameLogic (GameType (..))
import qualified Network.WebSockets as WS
import Server.Messages
import Users.User (UserId(..), UserRepo (..), User (..), RegUId (..))
import Utils.Utils
import Network.WebSockets (Connection)




processMsgLogInOut :: UserRepo urepo => urepo -> Connection -> LogInOut -> IO (Maybe RegUId)
processMsgLogInOut repo conn (Login username password) = do
  mbUser <- findUserByUsername repo username
  case mbUser of
    Nothing -> sendWebSocketOutputMessage conn LoginErrorMsg >> pure Nothing
    Just User{userId} -> do
      case userId of
        RegUserId regUid -> do
          passOk <- checkPassword repo regUid password
          if passOk
            then pure $ Just regUid
            else sendWebSocketOutputMessage conn LoginErrorMsg >> pure Nothing
        AnonUserId _ -> do
          sendWebSocketOutputMessage conn LoginErrorMsg
          error "Server.MessageProcessor processMsgLogInOut: findUserByUsername returns User with AnonUserId"
processMsgLogInOut repo conn Logout = error "processMsgLogInOut Logout not implemented"
processMsgLogInOut repo conn (Register username password) = do
  res <- addUser repo username password
  case res of
    usrId@(Just (RegUId uId)) -> do 
      sendWebSocketOutputMessage conn $ RegisteredSuccessfullyMsg uId
      pure usrId
    Nothing -> do 
      sendWebSocketOutputMessage conn RegisterErrorMsg
      pure Nothing


processInitJoinRoom ::  UserId -> InitJoinRoom -> IO ()
processInitJoinRoom userId (InitGameRoom params) = do
  pure ()
  -- ConnThreadReader _ mvarRooms <- ask
  -- mbNewRoomId <- liftIO $ createGameRoom userId (extractGameType params) mvarRooms -- TODO get GameType
  -- case mbNewRoomId of
  --   Right newRoomId -> sendWebSocketOutputMessage (GameRoomCreatedMsg newRoomId)
  --   Left oldRoomId -> sendWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg oldRoomId)
processInitJoinRoom userId (JoinGameRoom roomId) = error "processInitJoinRoom JoinGameRoom not implemented"

processGameActionMsg :: GameAction -> IO ()
processGameActionMsg (GameAction params) = error "processGameActionMsg not implemented"

processIncorrectMsg :: Connection -> Text -> IO ()
processIncorrectMsg conn _ = sendWebSocketOutputMessage conn ResendIncorrectMsg

processUpdateExistingUser = error "processUpdateExistingUser not implemented"


sendWebSocketOutputMessage :: Connection -> WebSocketOutputMessage -> IO ()
sendWebSocketOutputMessage conn msg = do
  logger LgMessage (Text.unpack $ fromWebSocketOutputMessage msg)
  WS.sendTextData conn (fromWebSocketOutputMessage msg :: Text)

extractGameType :: [WSMsgFormat] -> GameType
extractGameType = error "extractGameType not implemented"

askForExistingUser :: Connection -> IO ()
askForExistingUser conn = sendWebSocketOutputMessage conn AskForExistingUserMsg


