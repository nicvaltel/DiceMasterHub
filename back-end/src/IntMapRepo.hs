{-# LANGUAGE NamedFieldPuns #-}

module IntMapRepo (IntMapRepo, empty, append, delete, clean, modify, lookup) where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Prelude hiding (lookup)

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
delete idx IntMapRepo {freeIds, maxId, intMap} =
  IntMapRepo
    { freeIds = idx : freeIds,
      maxId = maxId,
      intMap = IntMap.delete idx intMap
    }

modify :: (a -> a) -> Key -> IntMapRepo a -> IntMapRepo a
modify f idx  repo@IntMapRepo{intMap} = repo{intMap = IntMap.update (Just . f) idx intMap}

lookup :: Key -> IntMapRepo a -> Maybe a 
lookup idx IntMapRepo{intMap} = IntMap.lookup idx intMap

-- | Garbage collector for IntMapRepo - force empty freeIds and reorganize intMap
clean :: IntMapRepo a -> IntMapRepo a
clean = id -- TODO implement