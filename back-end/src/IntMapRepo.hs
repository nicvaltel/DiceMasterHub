{-# LANGUAGE NamedFieldPuns #-}

module IntMapRepo (IntMapRepo, empty, append, delete, clean) where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Key = Int

data IntMapRepo a = IntMapRepo
  { freeIds :: [Int],
    maxId :: Int,
    intMap :: IntMap a
  }

empty :: IntMapRepo a
empty =
  IntMapRepo
    { freeIds = [],
      maxId = 0,
      intMap = IntMap.empty
    }

append :: a -> IntMapRepo a -> (IntMapRepo a, Key)
append value IntMapRepo {freeIds, maxId, intMap} = do
  let (id', freeIds', maxId') = case freeIds of
        [] -> (maxId, freeIds, maxId + 1)
        (headId : restIds) -> (headId, restIds, maxId)
  let newIntMapRepo =
        IntMapRepo
          { freeIds = freeIds',
            maxId = maxId',
            intMap = IntMap.insert id' value intMap
          }
  (newIntMapRepo, id')

delete :: Key -> IntMapRepo a -> IntMapRepo a
delete id' IntMapRepo {freeIds, maxId, intMap} =
  IntMapRepo
    { freeIds = id' : freeIds,
      maxId = maxId,
      intMap = IntMap.delete id' intMap
    }

-- | Garbage collector for IntMapRepo - force empty freeIds and reorganize intMap
clean :: IntMapRepo a -> IntMapRepo a
clean = id -- TODO implement