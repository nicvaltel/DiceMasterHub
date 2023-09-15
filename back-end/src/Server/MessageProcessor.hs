module Server.MessageProcessor where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (ask)
import Data.Text (Text)
import qualified Data.Text as Text
import GameLogic.GameLogic (GameType (..))
import GameRoom.GameRoom
import qualified Network.WebSockets as WS
import Server.Messages
import Server.Types
import Users.User (UserId)
import Utils.Logger

processMsgLogInOut :: LogInOut -> ConnThread mvar ()
processMsgLogInOut (Login username) = undefined
processMsgLogInOut Logout = undefined

processInitJoinRoom :: (GameRoomsRepo mvar) => UserId -> InitJoinRoom -> ConnThread mvar ()
processInitJoinRoom userId (InitGameRoom params) = do
  ConnThreadReader _ mvarRooms <- ask
  mbNewRoomId <- liftIO $ createGameRoom userId (extractGameType params) mvarRooms -- TODO get GameType
  case mbNewRoomId of
    Right newRoomId -> sendWebSocketOutputMessage (GameRoomCreatedMsg newRoomId)
    Left oldRoomId -> sendWebSocketOutputMessage (GameRoomIsAlreadyActiveMsg oldRoomId)
processInitJoinRoom userId (JoinGameRoom roomId) = undefined

processGameActionMsg :: GameAction -> ConnThread mvar ()
processGameActionMsg (GameAction params) = undefined

processIncorrectMsg :: [Text] -> ConnThread mvar ()
processIncorrectMsg _ = sendWebSocketOutputMessage ResendIncorrectMsg

sendWebSocketOutputMessage :: WebSocketOutputMessage -> ConnThread mvar ()
sendWebSocketOutputMessage msg = do
  liftIO $ logger LgMessage (Text.unpack $ fromWebSocketOutputMessage msg)
  ConnThreadReader conn _ <- ask
  liftIO $ WS.sendTextData conn (fromWebSocketOutputMessage msg :: Text)

extractGameType :: [WSMsgFormat] -> GameType
extractGameType = undefined -- TODO implement