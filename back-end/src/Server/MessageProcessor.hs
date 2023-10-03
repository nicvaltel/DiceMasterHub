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
import Users.User 
import Utils.Utils
import Network.WebSockets (Connection)
import Server.WebSocketServerClass (WebSocketServer(..))
import Server.Connection (ConnectionId, ConnectionsRepo (updateUserId, addConn), ConnectionStatus (CSNormal))
import Text.Printf (printf)


userSessionLoginChanged :: (WebSocketServer wss c g u, ToAnyUserId r) => wss ->  ConnectionId -> UserId r -> IO ()
userSessionLoginChanged wss idConn userId = do
  updateUserId (getConnRepo wss) idConn userId
  -- TODO end users game room (active or not just started)


processMsgLogInOut :: WebSocketServer wss c g u => wss ->  Connection -> ConnectionId -> LogInOut -> IO ()
processMsgLogInOut wss conn idConn (Login username password) = do
  let uRepo = getUserRepo wss
  mbRegUser <- findUserByUsername uRepo username  
  case mbRegUser of
    Nothing -> sendWebSocketOutputMessage conn LoginErrorOutMsg
    Just User{userId} -> do
          passOk <- checkPassword uRepo userId password -- Here is problem
          if passOk
            then do
              userSessionLoginChanged wss idConn userId
              sendWebSocketOutputMessage conn $ LoginSuccessfullyOutMsg userId
            else sendWebSocketOutputMessage conn LoginErrorOutMsg

processMsgLogInOut wss conn idConn Logout = do
  userId <- checkForExistingUser wss conn
  userSessionLoginChanged wss idConn userId
  sendWebSocketOutputMessage conn $ LogoutSuccessfullyOutMsg userId

processMsgLogInOut wss conn idConn (Register username password) = do
  res <- addUser (getUserRepo wss) username password
  case res of
    Just userId@(UserId uId) -> do 
      sendWebSocketOutputMessage conn $ RegisteredSuccessfullyOutMsg uId
      userSessionLoginChanged wss idConn userId
      sendWebSocketOutputMessage conn $ LoginSuccessfullyOutMsg userId
    Nothing -> sendWebSocketOutputMessage conn RegisterErrorOutMsg



processInitJoinRoom :: UserId r -> InitJoinRoom -> IO ()
processInitJoinRoom userId (InitGameRoom params) = pure ()
--   do
--   ConnThreadReader _ mvarRooms <- ask
--   mbNewRoomId <- liftIO $ createGameRoom userId (extractGameType params) mvarRooms -- TODO get GameType
--   case mbNewRoomId of
--     Right newRoomId -> sendWebSocketOutputMessage (GameRoomCreatedMsg newRoomId)
--     Left oldRoomId -> sendWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg oldRoomId)
-- processInitJoinRoom userId (JoinGameRoom roomId) = error "processInitJoinRoom JoinGameRoom not implemented"

processGameActionMsg :: GameAction -> IO ()
processGameActionMsg (GameAction params) = error "processGameActionMsg not implemented"

processIncorrectMsg :: Connection -> Text -> IO ()
processIncorrectMsg conn _ = sendWebSocketOutputMessage conn ResendIncorrectOutMsg

processUpdateExistingUser = error "processUpdateExistingUser not implemented"


sendWebSocketOutputMessage :: Connection -> WebSocketOutputMessage -> IO ()
sendWebSocketOutputMessage conn msg = do
  logger LgMessage (Text.unpack $ fromWebSocketOutputMessage msg)
  WS.sendTextData conn (fromWebSocketOutputMessage msg :: Text)

extractGameType :: [WSMsgFormat] -> GameType
extractGameType = error "extractGameType not implemented"

askForExistingUser :: Connection -> IO ()
askForExistingUser conn = sendWebSocketOutputMessage conn AskForExistingUserOutMsg


