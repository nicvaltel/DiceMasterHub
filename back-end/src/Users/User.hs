{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Users.User where

import Data.Text (Text)

data RegStatus = Registered | Anonim
  deriving (Show, Eq, Ord)

newtype UserId (r :: RegStatus) = UserId Int
  deriving (Show, Eq, Ord)

data User (r :: RegStatus) = User {userId :: UserId r, userName :: Username}
  deriving (Show)

type Username = Text

type Password = Text

type AnyUserId = Either (UserId 'Anonim) (UserId 'Registered)

fromAnyUserId :: AnyUserId -> UserId (r :: RegStatus)
fromAnyUserId (Left (UserId uId)) = UserId uId
fromAnyUserId (Right (UserId uId)) = UserId uId

class ToAnyUserId (r :: RegStatus) where
  toAnyUserId :: UserId (r :: RegStatus) -> AnyUserId

instance ToAnyUserId 'Registered where
  toAnyUserId = Right

instance ToAnyUserId 'Anonim where
  toAnyUserId = Left

class UserRepo db where
  findUserById :: db -> UserId 'Registered -> IO (Maybe (User 'Registered))
  findUserByUsername :: db -> Username -> IO (Maybe (User 'Registered))
  addUser :: db -> Username -> Password -> IO (Maybe (UserId 'Registered)) -- TODO save hashed password
  updateUser :: db -> User 'Registered -> IO Bool
  deleteUser :: db -> UserId 'Registered -> IO Bool
  checkPassword :: db -> UserId 'Registered -> Password -> IO Bool -- TODO save hashed password


-- data UserId (r :: RegStatus) where
--   UserId :: ToAnyUserId r => Int -> UserId r

-- data User (r :: RegStatus) where
--   User :: ToAnyUserId r => UserId r -> Username -> User r


-- newtype ToAnyUserId r => UserId (r :: RegStatus) = UserId Int
--   deriving (Show, Eq, Ord)

-- data ToAnyUserId r => User (r :: RegStatus) = User {userId :: UserId r, userName :: Username}
--   deriving (Show)
