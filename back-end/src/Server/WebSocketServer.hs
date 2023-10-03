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

module Server.WebSocketServer where

import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever)
import qualified Data.Text as Text
import GameRoom.GameRoom (GameRoomRepo (..))
import qualified Network.WebSockets as WS
import Server.Connection
import Server.MessageProcessor
import Server.Messages
import Users.User (UserId (..), UserRepo, RegStatus (..))
import Utils.Utils (LgSeverity (..), logger)
import Server.WebSocketServerClass

checkForExistingUser' :: WebSocketServer wss c g u => wss -> WS.Connection -> IO (UserId 'Anonim)
checkForExistingUser' wss conn = do
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
  where
    -- finally
    --   (wsThreadMessageListener wss conn idConn)
    --   (disconnect idConn)

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
        LogInOutInMsg logMsg -> do
          processMsgLogInOut wss conn idConn logMsg 
          -- for debug
          connState <- lookupConnState (getConnRepo wss) idConn
          logger LgDebug $ show connState
          -- for debug
        InitJoinRoomInMsg ijrMsg -> do
          mbUserId <- userIdFromConnectionId connRepo idConn
          case mbUserId of
            Nothing -> undefined
            Just userId -> processInitJoinRoom userId ijrMsg
        GameActionInMsg gameActMsg -> processGameActionMsg gameActMsg
        AnswerExistingUserInMsg uId -> processUpdateExistingUser wss idConn uId
        IncorrectInMsg txt -> processIncorrectMsg conn txt

