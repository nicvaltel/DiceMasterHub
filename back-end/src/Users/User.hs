{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.User where

import Data.Text (Text)

data User = User {userId :: UserId, userName :: Username}

data UserId = UserId {unUserId :: Int} | AnonUserId {anonUserId :: Int}

type Username = Text

type Password = Text

-- type UserFrom = User

-- type UserTo = User

class UserRepo db where
  findUserById :: db -> UserId -> IO (Maybe User)
  addUser :: db -> Username -> Password -> IO (Maybe UserId)
  updateUser :: db -> User -> IO Bool
  deleteUser :: db -> UserId -> IO Bool
  checkPassword :: db -> UserId -> Password -> IO Bool
