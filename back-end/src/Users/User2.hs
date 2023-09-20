{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.User2 where

import Data.Text (Text)
import Language.Haskell.TH
import Control.Monad
import Data.Typeable (typeOf, Typeable)

newtype UserId (r :: RegStatus) = UserId { unUserId :: Int }
  deriving (Show)

data RegStatus = Registered | Anonim
  deriving (Show, Typeable)


fff :: IO ()
fff = do
  putStrLn $ show $ typeOf registeredUserId --(undefined :: UserId 'Registered)

-- Example usage:
registeredUserId :: UserId 'Registered
registeredUserId = UserId 42

anonimUserId :: UserId 'Anonim
anonimUserId = UserId 123