{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
-- websocat -v ws://127.0.0.1:1234
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.WebSocketServerClass
  ( PingTime,
    WebSocketServer (..),
  )
where

import Control.Exception (finally, SomeException, catch)
import Control.Monad (forever)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomRepo (..))
import qualified Network.WebSockets as WS
import Server.Connection
import Server.MessageProcessor
import Server.Messages
import Users.User (RegisteredUser (..), UserId (..), UserRepo)
import Utils.Utils (LgSeverity (..), logger)

type PingTime = Int

class (ConnectionsRepo crepo, GameRoomRepo grrepo, UserRepo urepo) => WebSocketServer wss crepo grrepo urepo | wss -> crepo grrepo urepo where
  getConnRepo :: wss -> crepo
  getGameRoomRepo :: wss -> grrepo
  getUserRepo :: wss -> urepo

  webSocketServer :: wss -> PingTime -> WS.ServerApp
  webSocketServer = webSocketServer'

  wsThreadMessageListener :: wss -> WS.Connection -> ConnectionId -> IO ()
  wsThreadMessageListener = wsThreadMessageListener'

checkForExistingUser :: WebSocketServer wss c g u => wss -> WS.Connection -> IO (UserId 'Anonim)
checkForExistingUser wss conn = do
  uId <- nextAnonUserId (getConnRepo wss)
  askForExistingUser conn
  pure uId

webSocketServer' :: WebSocketServer wss c g u => wss -> PingTime -> WS.ServerApp
webSocketServer' wss pingTime = \pending -> do
  
  conn <- WS.acceptRequest pending
  userId <- checkForExistingUser wss conn
  idConn <- addConn (getConnRepo wss) conn userId CSNormal
  logger LgInfo $ show idConn ++ " connected"

  -- for debug
  connState <- lookupConnState (getConnRepo wss) idConn
  logger LgDebug $ show connState
  -- for debug

  WS.withPingThread conn pingTime (pure ()) $ do
    catch
        (wsThreadMessageListener wss conn idConn)
        (\(e :: SomeException) -> (putStrLn $ "WebSocket thread error: " ++ show e) >> disconnect idConn)
    -- finally
    --   (wsThreadMessageListener wss conn idConn)
    --   (disconnect idConn)

  where
    disconnect idConn = do
      removeConn (getConnRepo wss) idConn
      logger LgInfo $ show idConn ++ " disconnected"

wsThreadMessageListener' :: WebSocketServer wss c g u => wss -> WS.Connection -> ConnectionId -> IO ()
wsThreadMessageListener' wss conn idConn =
  forever $ do
    (msg :: WSMsgFormat) <- WS.receiveData conn
    logger LgInfo $ "RECIEVE #(" <> show idConn <> "): " <> Text.unpack msg

    -- for debug
    connState <- lookupConnState (getConnRepo wss) idConn
    logger LgDebug $ show connState
    -- for debug


    connStatus <- getConnStatus connRepo idConn -- if connection status is not found, there will be CSConnectionNotFound
    case connStatus of -- FSM switcher
      CSNormal -> normalMessageProcessor msg
      CSConnectionNotFound -> logger LgError $ "ConnectionId not found, but message recieved idConn = " ++ show idConn
    pure ()
  where
    connRepo = getConnRepo wss
    userRepo = getUserRepo wss

    normalMessageProcessor :: WSMsgFormat -> IO ()
    normalMessageProcessor msg = do
      let wsMsg = toWebSocketInputMessage msg
      logger LgInfo (show wsMsg)
      case (toWebSocketInputMessage msg) of
        LogInOutMsg logMsg -> do
          mbUid <- processMsgLogInOut userRepo conn logMsg
          case mbUid of
            Just uId -> do
              updateUser connRepo idConn uId
              -- for debug
              connState <- lookupConnState (getConnRepo wss) idConn
              logger LgDebug $ show connState
              -- for debug
            Nothing -> pure ()
        InitJoinRoomMsg ijrMsg -> do
          mbUserId <- userIdFromConnectionId connRepo idConn
          case mbUserId of
            Nothing -> undefined
            Just userId -> processInitJoinRoom userId ijrMsg
        GameActionMsg gameActMsg -> processGameActionMsg gameActMsg
        AnswerExistingUserMsg uId -> processUpdateExistingUser wss idConn uId
        IncorrectMsg txt -> processIncorrectMsg conn txt
        