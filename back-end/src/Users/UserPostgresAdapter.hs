{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Users.UserPostgresAdapter (UserRepoDB (..)) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import qualified PostgreSQLConnector as PG
import Users.User

newtype UserRepoDB = UserRepoDB {poolConn :: Pool Connection}

instance UserRepo UserRepoDB where
  findUserById :: UserRepoDB -> (UserId 'Registered) -> IO (Maybe (User 'Registered))
  findUserById (UserRepoDB poolConn) regUId = findUserById' poolConn regUId

  findUserByUsername :: UserRepoDB -> Username -> IO (Maybe (User 'Registered))
  findUserByUsername (UserRepoDB poolConn) username = findUserByUsername' poolConn username

  addUser :: UserRepoDB -> Username -> Password -> IO (Maybe (UserId 'Registered))
  addUser (UserRepoDB poolConn) username passwd = addUser' poolConn username passwd

  updateUser :: UserRepoDB -> (User 'Registered) -> IO Bool
  updateUser (UserRepoDB poolConn) newUser = updateUser' poolConn newUser

  deleteUser :: UserRepoDB -> (UserId 'Registered) -> IO Bool
  deleteUser (UserRepoDB poolConn) uId = deleteUser' poolConn uId

  checkPassword :: UserRepoDB -> (UserId 'Registered) -> Password -> IO Bool
  checkPassword (UserRepoDB poolConn) uId passwd = checkPassword' poolConn uId passwd

findUserById' :: Pool Connection -> (UserId 'Registered) -> IO (Maybe (User 'Registered))
findUserById' poolConn userId@(UserId uId) = do
  result :: [Only Text] <- PG.withDBConn poolConn $ \conn -> query conn queryStr (Only uId)
  case result of
    [Only userName] -> pure $ Just User {userId, userName}
    _ -> pure Nothing
  where
    queryStr = "SELECT username FROM dice_master_hub.users WHERE id = ?"

findUserByUsername' :: Pool Connection -> Username -> IO (Maybe (User 'Registered))
findUserByUsername' poolConn userName = do
  result :: [Only Int] <- PG.withDBConn poolConn $ \conn -> query conn queryStr (Only userName)
  case result of
    [Only uId] -> pure $ Just User {userId = UserId uId, userName}
    _ -> pure Nothing
  where
    queryStr = "SELECT id FROM dice_master_hub.users WHERE username = ?"

addUser' :: Pool Connection -> Username -> Password -> IO (Maybe (UserId 'Registered))
addUser' poolConn userName passwd = do
  res :: [Only Int] <- PG.withDBConn poolConn $ \conn -> query conn queryStr (userName, passwd)
  case res of
    [Only uId] -> pure $ Just (UserId uId)
    _ -> pure Nothing
  where
    queryStr = "INSERT INTO dice_master_hub.users (username, passwd, created) VALUES(?, crypt(?, gen_salt('bf')), (now() AT TIME ZONE 'utc'::text)) returning id;"

checkPassword' :: Pool Connection -> (UserId 'Registered) -> Password -> IO Bool
checkPassword' poolConn (UserId uId) passwd = do
  res :: [Only Int] <- PG.withDBConn poolConn $ \conn -> query conn queryStr (uId, passwd)
  case res of
    [Only uId] -> pure True
    _ -> pure False
  where
    queryStr = "SELECT id FROM dice_master_hub.users WHERE id = ? and passwd = crypt(?, passwd)"

deleteUser' :: Pool Connection -> (UserId 'Registered) -> IO Bool
deleteUser' = undefined

updateUser' :: Pool Connection -> (User 'Registered) -> IO Bool
updateUser' = undefined
