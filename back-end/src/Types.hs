{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import qualified Data.Text as Text

data Timestamp



type WSMsgFormat = Text

data WebSocketInputMessage = LogInOutMsg LogInOut | InitJoinRoomMsg InitJoinRoom | GameActionMsg GameAction | IncorrectMsg [WSMsgFormat]

data LogInOut = Login [WSMsgFormat] | Logout

data InitJoinRoom = InitGameRoom [WSMsgFormat] | JoinGameRoom WSMsgFormat

data GameAction = GameAction [WSMsgFormat]

data WebSocketOutputMessage = ResendIncorrectMsg | RoomWaitingForParticipantMsg

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
  fromWebSocketOutputMessage RoomWaitingForParticipantMsg = "RoomWaitingForParticipant\n"
