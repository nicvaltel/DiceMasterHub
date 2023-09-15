module Server.Types where

import Control.Monad.RWS (RWST)
import Data.Text (Text)
import GameRoom.GameRoom (RoomId, RoomsMap)
import IntMapRepo (IntMapRepo)
import qualified Network.WebSockets as WS
import Users.User (UserId)

data WebSocketServerState = WebSocketServerState
  { wsConnectionRepo :: IntMapRepo ConnectionState
  }

data ConnectionState = ConnectionState {connStateConnection :: WS.Connection, connStateUserId :: Maybe UserId}

type ConnectionId = Int

data ConnThreadReader mvar = ConnThreadReader
  { connThreadConnection :: WS.Connection,
    connThreadRoomsRepo :: mvar RoomsMap
  }

data ConnThreadState = ConnThreadState {connStateRoom :: Maybe RoomId}

type ConnThreadWriter = Text

type ConnThread mvar a = RWST (ConnThreadReader mvar) ConnThreadWriter ConnThreadState IO a