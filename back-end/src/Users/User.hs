{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Users.User where

import Data.Text (Text)
import Data.Data (Typeable)


data User (r :: RegStatus) =  User {userId :: UserId r, userName :: Username}
  deriving (Show)

newtype UserId (r :: RegStatus) = UserId {unUserId :: Int}
  deriving (Show)

data RegStatus = Registered | Anonim
  deriving (Show)
 






-- class GetRegStatus a where
--   getRegStatus :: a -> RegStatus

-- instance GetRegStatus RegStatus where
--   getRegStatus Registered = Registered
--   getRegStatus Anonim = Anonim

-- instance GetRegStatus (User (r :: RegStatus)) where
--   getRegStatus :: User r -> RegStatus
--   getRegStatus user = case getRegStatus r of
--     _ -> Registered
--     Anonim -> Anonim


-- instance GetRegStatus (UserId 'Anonim) where
--   getRegStatus :: UserId 'Anonim -> RegStatus
--   getRegStatus _ = Anonim

-- getRegStatus' :: UserId r -> RegStatus
-- getRegStatus' _ = case (undefined :: r) of
--   Registered -> Registered
--   Anonim -> Anonim





-- instance Show (User 'Registered) where
--   show User {userId, userName } =
--     printf "User {userId = %s, userName = %s }" (show userId) (show userName)

-- instance Show (User 'Anonim) where
--   show User {userId, userName } =
--     printf "User {userId = %s, userName = %s }" (show userId) (show userName)
-- instance Show (UserId 'Anonim) where
--   show (UserId uId) = "Anonim UserId = " ++ show uId

type Username = Text

type Password = Text

-- type UserFrom = User

-- type UserTo = User

class UserRepo db where
  findUserById :: db -> UserId 'Registered -> IO (Maybe (User 'Registered))
  findUserByUsername :: db -> Username -> IO (Maybe (User 'Registered))
  addUser :: db -> Username -> Password -> IO (Maybe (UserId 'Registered)) -- TODO save hashed password
  updateUser :: db -> User 'Registered -> IO Bool
  deleteUser :: db -> UserId 'Registered -> IO Bool
  checkPassword :: db -> UserId 'Registered -> Password -> IO Bool -- TODO save hashed password
