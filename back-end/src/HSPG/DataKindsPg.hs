{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module HSPG.DataKindsPg where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data Temp (u :: TempUnit) = Temp Double

data TempUnit = C | K

instance Show (Temp 'C) where
    show (Temp t) = show t ++ "oC"

instance Show (Temp 'K) where
    show (Temp t) = show t ++ "oK"

data TempContainer = forall u. TempContainer (Map Int (Temp u))


-- testFunc :: IO ()
-- testFunc = do
--     let tm = TempContainer Map.empty
--     let tm1 = Map.insert 1 (Temp 7) tm
--     pure ()