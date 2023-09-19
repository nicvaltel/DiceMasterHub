{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Users.UserPostgresAdapter(UserRepoDB(..)) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only (Only), execute, formatQuery, query)
import qualified PostgreSQLConnector as PG
import Users.User

newtype UserRepoDB = UserRepoDB {poolConn :: Pool Connection}

instance UserRepo UserRepoDB where
  findUserById :: UserRepoDB -> UserId r -> IO (Maybe (User r))
  findUserById (UserRepoDB poolConn) uId = findUserById' poolConn uId

  addUser :: UserRepoDB -> Username -> Password -> IO (Maybe (UserId r))
  addUser (UserRepoDB poolConn) username passwd = addUser' poolConn username passwd

  updateUser :: UserRepoDB -> User r-> IO Bool
  updateUser (UserRepoDB poolConn) newUser = updateUser' poolConn newUser

  deleteUser :: UserRepoDB -> UserId r -> IO Bool
  deleteUser (UserRepoDB poolConn) uId = deleteUser' poolConn uId

  checkPassword :: UserRepoDB -> UserId r-> Password -> IO Bool
  checkPassword (UserRepoDB poolConn) uId passwd = checkPassword' poolConn uId passwd

findUserById' :: Pool Connection -> UserId r -> IO (Maybe (User r))
findUserById' poolConn userId@(UserId uId) = do
  result :: [Only (Text)] <- PG.withDBConn poolConn $ \conn -> query conn queryStr (Only uId)
  case result of
    [Only userName] -> pure $ Just User {userId, userName}
    _ -> pure Nothing
  where
    queryStr = "SELECT username FROM dice_master_hub.users where id = ?"

addUser' :: Pool Connection -> Username -> Password -> IO (Maybe (UserId r))
addUser' poolConn userName passwd = do
  res :: [(Only Int)] <- PG.withDBConn poolConn $ \conn -> query conn queryStr (userName, passwd)
  case res of
    [Only uId] -> pure $ Just (UserId uId)
    _ -> pure Nothing
  where
    queryStr = "INSERT INTO dice_master_hub.users (username, passwd, created) VALUES(?, ?, (now() AT TIME ZONE 'utc'::text)) returning id;"

checkPassword' :: Pool Connection -> UserId r -> Password -> IO Bool
checkPassword' = undefined

deleteUser' :: Pool Connection -> UserId r -> IO Bool
deleteUser' = undefined

updateUser' :: Pool Connection -> User r -> IO Bool
updateUser' = undefined
