{-# LANGUAGE OverloadedStrings #-}

module Server.Messages where

import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom
import Users.User (Password, Username)
import Utils.Utils (tReadMaybe, tshow)

type WSMsgFormat = Text

data WebSocketInputMessage
  = LogInOutMsg LogInOut
  | InitJoinRoomMsg InitJoinRoom
  | GameActionMsg GameAction
  | IncorrectMsg WSMsgFormat
  | AnswerExistingUserMsg AnswerExistingUser
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
  = ResendIncorrectMsg
  | GameRoomCreatedMsg RoomId
  | GameRoomIsAlreadyActiveMsg RoomId -- unable to create new gameroom before closing the old one
  | AskForExistingUserMsg
  | RegisterErrorMsg
  | RegisteredSuccessfullyMsg Int -- UserId
  | LoginErrorMsg

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
          [] -> IncorrectMsg txt
          ["Login", username, passwd] -> LogInOutMsg $ Login username passwd
          ["Logout"] -> LogInOutMsg Logout
          ["Register", username, password] -> LogInOutMsg (Register username password)
          ("Init" : params) -> InitJoinRoomMsg (InitGameRoom params)
          ["Join", roomId] -> InitJoinRoomMsg (JoinGameRoom roomId)
          ("GameAct" : params) -> GameActionMsg (GameAction params)
          ("AnswerExistingUser" : rest) -> maybeToWSIM $ toWSIMAnswerExistingUser rest
          _ -> IncorrectMsg txt
      where
        maybeToWSIM :: Maybe WebSocketInputMessage-> WebSocketInputMessage
        maybeToWSIM mbWSIM = maybe (IncorrectMsg txt) id mbWSIM

  fromWebSocketOutputMessage ResendIncorrectMsg = "Resend"
  fromWebSocketOutputMessage (GameRoomCreatedMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId
  fromWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId
  fromWebSocketOutputMessage AskForExistingUserMsg = "AskForExistingUser"
  fromWebSocketOutputMessage RegisterErrorMsg = "RegisterError"
  fromWebSocketOutputMessage (RegisteredSuccessfullyMsg userId) = "RegisteredSuccessfully;" <> tshow userId
  fromWebSocketOutputMessage LoginErrorMsg = "LoginError"



toWSIMAnswerExistingUser :: [Text] -> Maybe WebSocketInputMessage
toWSIMAnswerExistingUser ["ExistingAnon", uIdtxt] =
  case (tReadMaybe uIdtxt :: Maybe Int) of
    Just uId -> Just $ AnswerExistingUserMsg (ExistingAnon uId)
    Nothing -> Nothing
toWSIMAnswerExistingUser ["ExistingRegisteredUser", uIdtxt, password] =
  case (tReadMaybe uIdtxt :: Maybe Int) of
    Just uId -> Just $ AnswerExistingUserMsg (ExistingRegisteredUser uId password)
    Nothing -> Nothing
toWSIMAnswerExistingUser ["NonExistingUser"] = Just $ AnswerExistingUserMsg NonExistingUser
toWSIMAnswerExistingUser _ = Nothing