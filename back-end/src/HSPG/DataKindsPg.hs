{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}


module HSPG.DataKindsPg where

data Temp (u :: TempUnit) = Temp Double

data TempUnit = C | K
    deriving Show

instance Show (Temp 'C) where
    show (Temp t) = show t ++ "oC"

instance Show (Temp 'K) where
    show (Temp t) = show t ++ "oK"


data TempContainer where
  TempContainer :: Show (Temp u) =>  Temp u -> TempContainer

instance Show TempContainer where
    show (TempContainer t ) = "TempContainer " ++ show t



updateTempContainer :: Show (Temp u) => TempContainer -> Temp u -> TempContainer
updateTempContainer (TempContainer _ ) newTemp = TempContainer newTemp




-- data TempContainer = forall u. TempContainer {tcTemp :: (Temp (u :: TempUnit))}

-- updateTempContainer :: TempContainer -> Temp u -> TempContainer
-- updateTempContainer tc newTemp = tc{tcTemp = newTemp}


-- {-# LANGUAGE ExistentialQuantification #-}

-- instance Show (Temp 'C) where
--     show (Temp t) = show t ++ "oC"

-- instance Show (Temp 'K) where
--     show (Temp t) = show t ++ "oK"