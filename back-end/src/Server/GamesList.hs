{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.GamesList where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar
  ( TMVar,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryTakeTMVar,
  )
import Control.Monad (forever, replicateM)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import System.Random (randomRIO)
import Data.Char (toUpper)

type Username = Text

data GameVariant = GameVariant {
  gameVarPlayer :: String,
  gameVarRating :: Int,
  gameVarTime :: String,
  gameVarMode :: String
} deriving (Show)

newtype ListOfGames = ListOfGames [GameVariant]
  deriving (Show)

type Client = (Username, WS.Connection)

gameVarToText :: GameVariant -> Text
gameVarToText GameVariant{gameVarPlayer,gameVarRating,gameVarTime,gameVarMode} = 
  Text.pack gameVarPlayer <> ";" <>
  Text.pack (show gameVarRating) <> ";" <>
  Text.pack gameVarTime <> ";" <>
  Text.pack gameVarMode

lgToText :: ListOfGames -> Text
lgToText (ListOfGames gs) = Text.intercalate "\n" (gameVarToText <$> gs)

main :: String -> Int -> IO ()
main address portNum = do
  gamesListTMVar <- newEmptyTMVarIO
  _ <- async $ messageGenerator gamesListTMVar
  putStrLn $ "Websocket server at " ++ address ++ ":" ++ show portNum
  WS.runServer address portNum $ application gamesListTMVar

application :: TMVar ListOfGames -> WS.ServerApp
application gamesListTMVar = \pending -> do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    forever $ do
      gamesList <- atomically $ readTMVar gamesListTMVar
      WS.sendTextData conn (lgToText gamesList)
      threadDelay 500_000

messageGenerator :: TMVar ListOfGames -> IO ()
messageGenerator tmvListOfGames = forever $ do
  let numStrings = 20
  randomGameVariants <- generateRandomGameVariants numStrings
  atomically $ writeTMVar tmvListOfGames $ ListOfGames randomGameVariants
  threadDelay 100_000
  where
    generateRandomGameVariants :: Int -> IO [GameVariant]
    generateRandomGameVariants numStrings =
      replicateM numStrings randomGameVar

    randomGameVar :: IO GameVariant
    randomGameVar = do
      gameVarPlayer <- do
        (h:rest) <- randomString 10
        pure (toUpper h:rest)
      gameVarRating <- randomRIO(20, 99)
      gameVarTime <- do
        (m :: Int) <- randomRIO(3, 15)
        (s :: Int) <- randomRIO(0,10)
        pure $ show m ++ "+" ++ show s 
      gameVarMode <- do
        (n :: Int) <- randomRIO(0, 1)
        case n of
          0 -> pure "Casual"
          1 -> pure "Rated"
          _ -> pure "..."
      pure GameVariant{gameVarPlayer, gameVarRating, gameVarTime, gameVarMode}

    randomString :: Int -> IO String
    randomString len = replicateM len randomChar

    randomChar = randomRIO ('a', 'z')

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new

-- writeMVar :: MVar a -> a -> IO ()
-- writeMVar mv new = do
--     isEmpty <- isEmptyMVar mv
--     if isEmpty
--         then putMVar mv new
--         else takeMVar mv >> putMVar mv new
