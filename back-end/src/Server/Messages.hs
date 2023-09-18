{-# LANGUAGE OverloadedStrings #-}

module Server.Messages where

import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom
import Users.User (Username, Password)
import Utils.Utils (tshow)

type WSMsgFormat = Text

data WebSocketInputMessage = LogInOutMsg LogInOut | InitJoinRoomMsg InitJoinRoom | GameActionMsg GameAction | IncorrectMsg [WSMsgFormat]
  deriving (Show)

data LogInOut = Login [WSMsgFormat] | Logout | Register Username Password
  deriving (Show)

data InitJoinRoom = InitGameRoom [WSMsgFormat] | JoinGameRoom WSMsgFormat
  deriving (Show)

data GameAction = GameAction [WSMsgFormat]
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
          _ -> IncorrectMsg txts

  fromWebSocketOutputMessage ResendIncorrectMsg = "Resend;"
  fromWebSocketOutputMessage (GameRoomCreatedMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId <> ";"
  fromWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg roomId) = "RoomWaitingForParticipant;" <> tshow roomId <> ";"
  fromWebSocketOutputMessage AskForExistingUserMsg = "AskForExistingUser;"
  fromWebSocketOutputMessage RegisterErrorMsg = "RegisterError;"
  fromWebSocketOutputMessage (RegisteredSuccessfullyMst userId) = "RegisteredSuccessfully;" <> tshow userId <> ";"

  

