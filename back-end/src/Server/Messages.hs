{-# LANGUAGE OverloadedStrings #-}

module Server.Messages where

import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom

type WSMsgFormat = Text

data WebSocketInputMessage = LogInOutMsg LogInOut | InitJoinRoomMsg InitJoinRoom | GameActionMsg GameAction | IncorrectMsg [WSMsgFormat]

data LogInOut = Login [WSMsgFormat] | Logout

data InitJoinRoom = InitGameRoom [WSMsgFormat] | JoinGameRoom WSMsgFormat

data GameAction = GameAction [WSMsgFormat]

data WebSocketOutputMessage
  = ResendIncorrectMsg
  | GameRoomCreatedMsg RoomId
  | GameRoomIsAlreadyActiveMsg RoomId -- unable to create new gameroom before closing the old one
  | AskForExistingUserMsg

class WebSocketMSG a where
  toWebSocketInputMessage :: a -> WebSocketInputMessage
  fromWebSocketOutputMessage :: WebSocketOutputMessage -> a

-- fromWebSocketMessage :: a -> WebSocketMessage

instance WebSocketMSG Text where
  toWebSocketInputMessage txt =
    let txts = Text.lines txt
     in case txts of
          [] -> IncorrectMsg []
          ("Login" : params) -> LogInOutMsg (Login params) -- TODO password processing
          ["Logout"] -> LogInOutMsg Logout
          ("Init" : params) -> InitJoinRoomMsg (InitGameRoom params)
          ["Join", roomId] -> InitJoinRoomMsg (JoinGameRoom roomId)
          ("GameAct" : params) -> GameActionMsg (GameAction params)
          _ -> IncorrectMsg txts

  fromWebSocketOutputMessage ResendIncorrectMsg = "Resend\n"
  fromWebSocketOutputMessage (GameRoomCreatedMsg roomId) = "RoomWaitingForParticipant\n" <> Text.pack (show roomId) <> "\n"
  fromWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg roomId) = "RoomWaitingForParticipant\n" <> Text.pack (show roomId) <> "\n"
  fromWebSocketOutputMessage AskForExistingUserMsg = "AskForExistingUser\n"
