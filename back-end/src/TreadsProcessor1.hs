module TreadsProcessor1 where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Text.Printf(printf)
import qualified Data.IntMap.Strict as IntMap
import System.Random (randomRIO)

type Message = String

maxN :: Int
maxN = 10

main :: IO ()
main = do

  inputMessageQueue <- newTQueueIO
  _ <- async $ messageGenerator inputMessageQueue


  -- Create a list of shared queues for messages
  messageQueuesList <- zip [0..] <$> replicateM maxN newTQueueIO

  let messageQueues = IntMap.fromList messageQueuesList

  -- Spawn 10 worker threads, each with its own queue
  workers <- forM messageQueuesList $ \(idn, queue) -> async $ workerThread idn queue

  -- Main loop to process incoming messages
  void $ forever $ do
    (threadIndex, msg) <- atomically $ readTQueue inputMessageQueue
      -- Put the message into the queue of the selected thread
    atomically $ writeTQueue ((IntMap.!) messageQueues threadIndex) msg

  -- Wait for all worker threads to finish
  mapM_ wait workers

messageGenerator :: TQueue (Int, Message) -> IO ()
messageGenerator inputMessageQueue = forever $ do
  n <- randomRIO (0,maxN - 1)
  let msg = "MESSAGE (" ++ show n ++ ")"
  atomically $ writeTQueue inputMessageQueue (n, msg)
  threadDelay 100000


-- Worker thread function
workerThread :: Int -> TQueue Message -> IO ()
workerThread idn queue = forever $ do
  msg <- atomically $ readTQueue queue
  putStrLn $ printf "Thread #%d Processing message:  %s" idn msg
