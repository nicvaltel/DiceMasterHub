module Server.Types where


import Control.Concurrent (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar)
import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    atomically,
    newTMVarIO,
    newTQueueIO,
    putTMVar,
    takeTMVar,
    writeTQueue,
  )
import Control.Exception (finally)
import Control.Monad (forever)
-- import Network.WebSockets (Connection)

import Control.Monad.RWS (RWST (runRWST))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomsRepo (..), RoomId, RoomsMap)
import IntMapRepo (IntMapRepo)
import qualified IntMapRepo
import Utils.Logger (LgSeverity (LgInfo, LgMessage), logger)
import qualified Network.WebSockets as WS
import Server.Messages
import GameLogic.GameLogic (GameType(..))

data WebSocketServerState

-- data WebSocketServerState = WebSocketServerState
--   { wsConnectionRepo :: IntMapRepo ConnectionState
--   }

-- data ConnectionState = ConnectionState
--   { connStateConnection :: WS.Connection,
--     connStateMessageQueue :: TQueue WSMsgFormat
--   }

type ConnectionId = Int

data ConnThreadReader mvar = ConnThreadReader
  { connThreadConnection :: WS.Connection,
    connThreadRoomsRepo :: mvar RoomsMap
  }

data ConnThreadState = ConnThreadState {connStateRoom :: Maybe RoomId}

type ConnThreadWriter = Text

type ConnThread mvar a = RWST (ConnThreadReader mvar) ConnThreadWriter ConnThreadState IO a