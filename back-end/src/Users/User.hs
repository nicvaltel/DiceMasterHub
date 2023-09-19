{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Users.User where

import Data.Text (Text)

data User (r :: RegisteredUser) = User {userId :: UserId r, userName :: Username}

newtype UserId (r :: RegisteredUser) = UserId {unUserId :: Int}

data RegisteredUser = Registered | Anonim

type Username = Text

type Password = Text

-- type UserFrom = User

-- type UserTo = User

class UserRepo db where
  findUserById :: db -> UserId 'Registered -> IO (Maybe (User 'Registered))
  addUser :: db -> Username -> Password -> IO (Maybe (UserId 'Registered))
  updateUser :: db -> User 'Registered -> IO Bool
  deleteUser :: db -> UserId 'Registered-> IO Bool
  checkPassword :: db -> UserId 'Registered -> Password -> IO Bool
