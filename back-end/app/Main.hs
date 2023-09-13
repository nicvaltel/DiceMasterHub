module Main (main) where

import System.Environment (getArgs)
-- import qualified TreadsProcessor1
-- import qualified Websocket.Server
-- import qualified Websocket.ServerSimple
-- import qualified Websocket.GamesList
import qualified RunWebSocketServerTMVar

main :: IO ()
main = do 
    -- TreadsProcessor1.main

    -- Websocket.ServerSimple.main

    -- args <- getArgs
    -- if (length args /= 2)
    --     then putStrLn "Usage: back-end-exe address port"
    --     else do
    --         -- let address = "127.0.0.1" :: String 
    --         -- let portNum = 1234 :: Int
    --         let address = args !! 0 :: String 
    --         let portNum = read (args !! 1) :: Int
    --         Websocket.GamesList.main address portNum

    RunWebSocketServerTMVar.main