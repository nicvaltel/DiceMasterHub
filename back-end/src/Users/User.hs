{-# LANGUAGE OverloadedStrings #-}

module Users.User where

import Data.Text (Text)

newtype RegUId = RegUId Int
  deriving (Show, Eq, Ord)

newtype AnonUId = AnonUId Int
  deriving (Show, Eq, Ord)

data UserId = RegUserId RegUId | AnonUserId AnonUId
  deriving (Show, Eq, Ord)

data UserData = UserData {userName :: Username}
  deriving (Show)

data RegUser =  RegUser {regUId :: RegUId, regUserData :: UserData}
  deriving (Show)

data AnonUser = AnonUser {anonUId :: AnonUId, anonUserData :: UserData}
  deriving (Show)

data User = RegisteredUser RegUser | AnonimUser AnonUser
  deriving (Show)


-- data RegStatus = Registered | Anonim
--   deriving (Show)

type Username = Text

type Password = Text

class UserRepo db where
  findUserById :: db -> RegUId -> IO (Maybe RegUser)
  findUserByUsername :: db -> Username -> IO (Maybe RegUser)
  addUser :: db -> Username -> Password -> IO (Maybe RegUId) -- TODO save hashed password
  updateUser :: db -> RegUser -> IO Bool
  deleteUser :: db -> RegUId -> IO Bool
  checkPassword :: db -> RegUId -> Password -> IO Bool -- TODO save hashed password
