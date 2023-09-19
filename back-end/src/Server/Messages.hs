{-# LANGUAGE OverloadedStrings #-}

module Server.Messages where

import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom
import Users.User (Password, Username)
import Utils.Utils (tshow, tReadMaybe)

type WSMsgFormat = Text

data WebSocketInputMessage
  = LogInOutMsg LogInOut
  | InitJoinRoomMsg InitJoinRoom
  | GameActionMsg GameAction
  | IncorrectMsg [WSMsgFormat]
  | AnswerExistingUserMsg AnswerExistingUser
  deriving (Show)

data LogInOut = Login [WSMsgFormat] | Logout | Register Username Password
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
  | RegisteredSuccessfullyMst Int -- UserId

class WebSocketMSG a where
  toWebSocketInputMessage :: a -> WebSocketInputMessage
  fromWebSocketOutputMessage :: WebSocketOutputMessage -> a

-- fromWebSocketMessage :: a -> WebSocketMessage

instance WebSocketMSG Text where
  toWebSocketInputMessage txt =
    -- let txts = Text.lines txt
    let txts = Text.splitOn ";" txt
     in case txts of
          [] -> IncorrectMsg []
          ("Login" : params) -> LogInOutMsg (Login params) -- TODO password processing
          ["Logout"] -> LogInOutMsg Logout
          ["Register", username, password] -> LogInOutMsg (Register username password)
          ("Init" : params) -> InitJoinRoomMsg (InitGameRoom params)
          ["Join", roomId] -> InitJoinRoomMsg (JoinGameRoom roomId)
          ("GameAct" : params) -> GameActionMsg (GameAction params)
          ["AnswerExistingUser", "ExistingAnon" , uIdtxt] -> 
            case (tReadMaybe uIdtxt :: Maybe Int) of
              Just uId -> AnswerExistingUserMsg (ExistingAnon uId)
              Nothing -> IncorrectMsg txts
          ["AnswerExistingUser", "ExistingRegisteredUser", uIdtxt, password] -> 
            case (tReadMaybe uIdtxt :: Maybe Int) of
              Just uId -> AnswerExistingUserMsg (ExistingRegisteredUser uId password)
              Nothing -> IncorrectMsg txts
          ["AnswerExistingUser", "NonExistingUser"] -> AnswerExistingUserMsg NonExistingUser
          _ -> IncorrectMsg txts

  fromWebSocketOutputMessage ResendIncorrectMsg = "Resend;"
  fromWebSocketOutputMessage (GameRoomCreatedMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId <> ";"
  fromWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId <> ";"
  fromWebSocketOutputMessage AskForExistingUserMsg = "AskForExistingUser;"
  fromWebSocketOutputMessage RegisterErrorMsg = "RegisterError;"
  fromWebSocketOutputMessage (RegisteredSuccessfullyMst userId) = "RegisteredSuccessfully;" <> tshow userId <> ";"
