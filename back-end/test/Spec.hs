{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified PostgreSQLSpec 
import Configuration.Dotenv (parseFile)

main :: IO ()
main = do
    env <- parseFile "config.env"
    hspec $ spec env

spec :: [(String, String)] -> Spec
spec env = do 
    describe "PostgreSQL" $ PostgreSQLSpec.spec env