module TreadsProcessor0 where

-- import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Hashable (hash)
import Text.Printf(printf)

type Message = String

main :: IO ()
main = do
  -- Create a list of shared queues for messages
  messageQueues <- replicateM 10 newTQueueIO

  -- Spawn 10 worker threads, each with its own queue
  workers <- forM (zip messageQueues [0..]) $ \(queue,idn) -> async $ workerThread idn queue

  -- Main loop to process incoming messages
  void $ forever $ do
    putStrLn "Enter a message (or 'quit' to exit):"
    msg <- getLine
    when (msg /= "quit") $ do
      -- Determine which thread to send the message to
      let threadIndex = hash msg `mod` 10

      -- Put the message into the queue of the selected thread
      atomically $ writeTQueue (messageQueues !! threadIndex) msg

  -- Wait for all worker threads to finish
  mapM_ wait workers

-- Worker thread function
workerThread :: Int -> TQueue Message -> IO ()
workerThread idn queue = forever $ do
  msg <- atomically $ readTQueue queue
  putStrLn $ printf "Thread #%d Processing message:  %s" idn msg

