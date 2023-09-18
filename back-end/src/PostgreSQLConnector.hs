{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgreSQLConnector
  ( DBConfig (..),
    readDBConfig,
    initDBConn,
    withDBConn,
  )
where

import Control.Exception (bracket)
import Data.Either.Combinators (maybeToRight)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Simple
  ( Connection,
    close,
    connectPostgreSQL,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationResult (MigrationError),
    runMigrations,
  )
import Text.Printf (printf)
import UnliftIO (throwString)

data DBConfig = DBConfig
  { dbHost :: String,
    dbPort :: Int,
    dbName :: String,
    dbUser :: String,
    dbPassword :: String,
    dbStripeCount :: Int,
    dbMaxOpenConnPerStripe :: Int,
    dbIdleConnTimeout :: Double
  }
  deriving (Show)

readDBConfig :: [(String, String)] -> Either String DBConfig
readDBConfig env = do
  dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
  dbPort <- maybeToRight "No port number defined" (read <$> lookup "POSTGRES_PORT" env)
  dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
  dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
  dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
  dbStripeCount <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_STRIPE_COUNT" env)
  dbMaxOpenConnPerStripe <- maybeToRight "No max open connections per stripe defined" (read <$> lookup "POSTGRES_MAX_OPEN_CONN_PER_STRIPE" env)
  dbIdleConnTimeout <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_IDLE_CONN_TIMEOUT" env)
  pure DBConfig {dbHost, dbPort, dbName, dbUser, dbPassword, dbStripeCount, dbMaxOpenConnPerStripe, dbIdleConnTimeout}

-- private function
migrate :: Pool Connection -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> pure ()
  where
    cmds =
      [ MigrationInitialization,
        MigrationDirectory "migration/PostgreSQL/Migrations_00_Create_Tables",
        MigrationDirectory "migration/PostgreSQL/Migrations_01_Fill_Tables"
      ]

-- private function
withPool :: DBConfig -> (Pool Connection -> IO a) -> IO a
withPool conf action = do
  bracket initPool cleanPool action
  where
    initPool = newPool $ defaultPoolConfig openConn close (dbIdleConnTimeout conf) (dbMaxOpenConnPerStripe conf)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (fromString connectString)

    connectString :: String
    connectString = printf "host='%s' port=%d dbname='%s' user='%s' password='%s'" (dbHost conf) (dbPort conf) (dbName conf) (dbUser conf) (dbPassword conf)

initDBConn :: [(String, String)] -> (Pool Connection  -> IO a) -> IO a
initDBConn env action =
  let cfg = getCfg
   in case cfg of
        Left err -> error err
        Right postgreCfg -> do
          withPool postgreCfg $ \poolConn -> do
            migrate poolConn
            action poolConn
  where
    getCfg :: Either String DBConfig
    getCfg = do
      pgCfg_ <- readDBConfig env
      pure pgCfg_

withDBConn :: Pool Connection -> (Connection -> IO a) -> IO a
withDBConn poolConn action = do
  withResource poolConn (\conn -> action conn)

-- newtype PoolBDConn = PoolBDConn {poolBDConn :: Pool Connection}



-- How To Run Example

-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- module RunDB where

-- import qualified PostgreSQLConnector as PG
-- import Configuration.Dotenv (parseFile)
-- import Data.Text (Text)
-- import Database.PostgreSQL.Simple (Only (Only), query)

-- runDB :: FilePath -> IO ()
-- runDB envFile = do
--   env <- parseFile envFile
--   PG.initDBConn env $ \poolDBConn -> do
--     putStrLn "DATABASE STARTED!!!"
--     result :: [(Int, Text, Int, Text)] <- PG.withDBConn poolDBConn $ \conn -> query conn qryStr (Only (2 :: Int))
--     case result of
--       [] -> pure ()
--       _ -> print result
--   where
--     qryStr = "SELECT id, name, price, description FROM public.tmp_test_table where id = ?"