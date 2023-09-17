{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.RunDB where

import qualified Adapter.PostgreSQL.Common as PG
import Configuration.Dotenv (parseFile)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only (Only), query)

runDB :: FilePath -> IO ()
runDB envFile = do
  cfg <- getCfg <$> parseFile envFile
  case cfg of
    Left err -> error err
    Right postgreCfg -> do
      PG.withAppState postgreCfg $ \pool -> do
        print "DATABASE STARTED!!!"
        result :: [(Int, Text, Int, Text)] <- PG.withDBConn pool $ \conn -> query conn qryStr (Only (2 :: Int))
        case result of
          [] -> pure ()
          _ -> print result

  where
    qryStr = "SELECT id, name, price, description FROM public.tmp_test_table where id = ?"

    getCfg :: [(String, String)] -> Either String PG.DBConfig
    getCfg env = do
      pgCfg_ <- PG.readDBConfig env
      pure pgCfg_