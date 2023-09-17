{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.User where

import Data.Text (Text)
import qualified Data.Text as Text

data User = User
  { userId :: UserId,
    userName :: Username
  }

mkUser userId username = User userId username

newtype UserId = UserId {unUserId :: Int}

type Username = Text

type Password = Text

-- type UserFrom = User

-- type UserTo = User

class UserRepo db where
  findUserById :: UserId -> db (Maybe User)
  addUser :: User -> db Bool
  updateUser :: User -> db Bool
  deleteUser :: UserId -> db Bool
  checkPassword :: UserId -> Password -> db Bool


newtype UserRepoDB a = UserRepoDB {runUserRepo :: IO a}

instance UserRepo UserRepoDB where
  findUserById :: UserId -> UserRepoDB (Maybe User)
  findUserById uId = undefined -- UserRepoDB (pure $ Just $ mkUser uId (Text.pack $ show uId))

  addUser :: User -> UserRepoDB Bool
  addUser user = UserRepoDB (pure True)

  
  

