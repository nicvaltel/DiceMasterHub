{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Server.Messages where

import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom
import Users.User (Password, Username, UserId (UserId), RegStatus (..))
import Utils.Utils (tReadMaybe, tshow)

type WSMsgFormat = Text

data WebSocketInputMessage
  = LogInOutInMsg LogInOut
  | InitJoinRoomInMsg InitJoinRoom
  | GameActionInMsg GameAction
  | IncorrectInMsg WSMsgFormat
  | AnswerExistingUserInMsg AnswerExistingUser
  deriving (Show)

data LogInOut = Login Username Password | Logout | Register Username Password
  deriving (Show)

data InitJoinRoom = InitGameRoom [WSMsgFormat] | JoinGameRoom WSMsgFormat
  deriving (Show)

data GameAction = GameAction [WSMsgFormat]
  deriving (Show)

data AnswerExistingUser = ExistingAnon Int | ExistingRegisteredUser Int Text | NonExistingUser
  deriving (Show)

data WebSocketOutputMessage
  = ResendIncorrectOutMsg
  | GameRoomCreatedOutMsg RoomId
  | GameRoomIsAlreadyActiveOutMsg RoomId -- unable to create new gameroom before closing the old one
  | AskForExistingUserOutMsg
  | RegisterErrorOutMsg
  | RegisteredSuccessfullyOutMsg Int -- UserId
  | LoginErrorOutMsg
  | LoginSuccessfullyOutMsg (UserId 'Registered)
  | LogoutSuccessfullyOutMsg (UserId 'Anonim)

class WebSocketMSG a where
  toWebSocketInputMessage :: a -> WebSocketInputMessage
  fromWebSocketOutputMessage :: WebSocketOutputMessage -> a

-- fromWebSocketMessage :: a -> WebSocketMessage

instance WebSocketMSG Text where
  toWebSocketInputMessage txt =
    -- let txts = Text.lines txt
    let cleanTxt = if Text.last txt == '\n' then Text.init txt else txt
        txts = Text.splitOn ";" cleanTxt
     in case txts of
          [] -> IncorrectInMsg txt
          ["Login", username, passwd] -> LogInOutInMsg $ Login username passwd
          ["Logout"] -> LogInOutInMsg Logout
          ["Register", username, password] -> LogInOutInMsg (Register username password)
          ("Init" : params) -> InitJoinRoomInMsg (InitGameRoom params)
          ["Join", roomId] -> InitJoinRoomInMsg (JoinGameRoom roomId)
          ("GameAct" : params) -> GameActionInMsg (GameAction params)
          ("AnswerExistingUser" : rest) -> maybeToWSIM $ toWSIMAnswerExistingUser rest
          _ -> IncorrectInMsg txt
      where
        maybeToWSIM :: Maybe WebSocketInputMessage-> WebSocketInputMessage
        maybeToWSIM mbWSIM = maybe (IncorrectInMsg txt) id mbWSIM

  fromWebSocketOutputMessage ResendIncorrectOutMsg = "Resend"
  fromWebSocketOutputMessage (GameRoomCreatedOutMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId
  fromWebSocketOutputMessage (GameRoomIsAlreadyActiveOutMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId
  fromWebSocketOutputMessage AskForExistingUserOutMsg = "AskForExistingUser"
  fromWebSocketOutputMessage RegisterErrorOutMsg = "RegisterError"
  fromWebSocketOutputMessage (RegisteredSuccessfullyOutMsg userId) = "RegisteredSuccessfully;" <> tshow userId
  fromWebSocketOutputMessage LoginErrorOutMsg = "LoginError"
  fromWebSocketOutputMessage (LoginSuccessfullyOutMsg (UserId uId)) = "LoginSuccessfullyMsg;" <> tshow uId
  fromWebSocketOutputMessage (LogoutSuccessfullyOutMsg (UserId uId)) = "LogoutSuccessfullyMsg;" <> tshow uId



toWSIMAnswerExistingUser :: [Text] -> Maybe WebSocketInputMessage
toWSIMAnswerExistingUser ["ExistingAnon", uIdtxt] =
  case (tReadMaybe uIdtxt :: Maybe Int) of
    Just uId -> Just $ AnswerExistingUserInMsg (ExistingAnon uId)
    Nothing -> Nothing
toWSIMAnswerExistingUser ["ExistingRegisteredUser", uIdtxt, password] =
  case (tReadMaybe uIdtxt :: Maybe Int) of
    Just uId -> Just $ AnswerExistingUserInMsg (ExistingRegisteredUser uId password)
    Nothing -> Nothing
toWSIMAnswerExistingUser ["NonExistingUser"] = Just $ AnswerExistingUserInMsg NonExistingUser
toWSIMAnswerExistingUser _ = Nothing