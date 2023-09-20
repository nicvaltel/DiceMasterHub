{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Users.User where

import Data.Text (Text)
import Text.Printf (printf)
import Data.Data (Proxy)

data User (r :: RegisteredUser) = User {userId :: UserId r, userName :: Username}
  deriving (Show)

newtype UserId (r :: RegisteredUser) = UserId {unUserId :: Int}
  deriving (Show)


-- instance Show (User 'Registered) where
--   show User {userId, userName } = 
--     printf "User {userId = %s, userName = %s }" (show userId) (show userName)

-- instance Show (User 'Anonim) where
--   show User {userId, userName } = 
--     printf "User {userId = %s, userName = %s }" (show userId) (show userName)


-- instance Show (UserId 'Registered) where
--   show (UserId uId) = "Registered UserId = " ++ show uId

-- instance Show (UserId 'Anonim) where
--   show (UserId uId) = "Anonim UserId = " ++ show uId


data RegisteredUser = Registered | Anonim

type Username = Text

type Password = Text

-- type UserFrom = User

-- type UserTo = User

class UserRepo db where
  findUserById :: db -> UserId 'Registered -> IO (Maybe (User 'Registered))
  findUserByUsername :: db -> Username -> IO (Maybe (User 'Registered))
  addUser :: db -> Username -> Password -> IO (Maybe (UserId 'Registered)) -- TODO save hashed password
  updateUser :: db -> User 'Registered -> IO Bool
  deleteUser :: db -> UserId 'Registered-> IO Bool
  checkPassword :: db -> UserId 'Registered -> Password -> IO Bool -- TODO save hashed password
