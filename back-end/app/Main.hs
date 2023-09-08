module Main (main) where

import Lib
import qualified TreadsProcessor1

main :: IO ()
main = do 
    someFunc
    TreadsProcessor1.main
