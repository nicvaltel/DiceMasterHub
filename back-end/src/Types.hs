module Types where
import Data.Text (Text)


data User

type UserFrom = User

type UserTo = User

data GameRoom

data GameRoomState

data GameRoomMessage

data WebSocketMessage = WebSocketMessage {wsm :: Text}
