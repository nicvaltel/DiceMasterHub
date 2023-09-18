
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgreSQLSpec where

import Test.Hspec

import qualified PostgreSQLConnector as PG
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only (Only), query, execute, execute_, query_)
import Control.Monad (replicateM_)
import Control.Concurrent.Async (async, wait)

spec :: [(String,String)] -> SpecWith ()
spec env = do
  it "Connecting to PostgreSQL and make a query" $ do    
    PG.initDBConn env $ \poolConn -> do
      putStrLn "DATABASE STARTED!!!"
      result :: [(Int, Text, Int, Text)] <- PG.withDBConn poolConn $ \conn -> query conn "SELECT id, name, price, description FROM public.tmp_test_table where id = ?" (Only (2 :: Int))
      result `shouldBe` [(2,"Sofa",20,"Description of sofa")]
  
  it "Running multiple queries at PostgreSQL" $ do
    PG.initDBConn env $ \poolConn -> do
      _ <- PG.withDBConn poolConn $ \conn -> execute_ conn "DELETE FROM dice_master_hub.test_tbl;"
      let querys = 
            map 
              (\n -> replicateM_ 100 $ PG.withDBConn poolConn $ \conn -> 
                execute conn "INSERT INTO dice_master_hub.test_tbl (test_text_field, test_int_fied) VALUES(?, ?);" ("Hello " <> show n, n))
              ([1 .. 100] :: [Int])
      as <- mapM async querys
      mapM_ wait as
      result :: [(Only Int)] <- PG.withDBConn poolConn $ \conn -> query_ conn "select count(*) from dice_master_hub.test_tbl"
      result `shouldBe` [(Only 10000)]
      _ <- PG.withDBConn poolConn $ \conn -> execute_ conn "DELETE FROM dice_master_hub.test_tbl;"
      pure ()


-- CREATE TEST TABLE
-- create table dice_master_hub.test_tbl (
--   id serial primary key,
--   test_text_field text,
--   test_int_fied bigint
-- );
