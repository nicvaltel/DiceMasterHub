{-# LANGUAGE OverloadedStrings #-}

module Users.User where

import Data.Text (Text)

data UserId = RegUserId Int | AnonUserId Int
  deriving (Show, Eq, Ord)

data User = User {userId :: UserId, userName :: Username}
  deriving (Show)

data RegStatus = Registered | Anonim
  deriving (Show)

type Username = Text

type Password = Text

class UserRepo db where
  findUserById :: db -> UserId -> IO (Maybe User)
  findUserByUsername :: db -> Username -> IO (Maybe User)
  addUser :: db -> Username -> Password -> IO (Maybe UserId) -- TODO save hashed password
  updateUser :: db -> User -> IO Bool
  deleteUser :: db -> UserId -> IO Bool
  checkPassword :: db -> UserId -> Password -> IO Bool -- TODO save hashed password
