module Data.LRUCache
  (
    LRUCache(..)
  , view
  , push
  , empty
  , clear
  , size
  ) where

import Data.List ( find
                 , findIndex )

data LRUCache k v = LRUCache { capacity :: Int -- ^ the capacity
                             , cache :: [(k, v)] -- ^ the cache content 
                             } deriving (Eq, Show, Read)

-- | get a value by key from a LRUCache.
view :: (Eq k) => k -> LRUCache k v -> (Maybe v, LRUCache k v)
view k (LRUCache n cache) = (fmap snd $ find ((== k) . fst) cache, LRUCache n $ k <|- cache)
  where (<|-) :: (Eq k) => k -> [(k, v)] -> [(k, v)]
        k' <|- xs = case findIndex ((== k') . fst) xs of
                      Nothing -> xs
                      Just i -> xs !! i : take i xs <> drop (succ i) xs

-- | push a cache iitem in a LRUCache.
push :: (k, v) -> LRUCache k v -> LRUCache k v
push e (LRUCache n cache) = LRUCache n . take n . (e:) $ cache

-- | create an empty LRUCache.
empty :: Int -> Maybe (LRUCache k v)
empty n
  | n <= 0 = Nothing
  | otherwise = Just $ LRUCache n []

-- | clear the LRUCache to empty.
clear :: LRUCache k v -> LRUCache k v
clear (LRUCache n _) = LRUCache n []

-- | get the LRUCache current cache size.
size :: LRUCache k v -> Int
size = length . cache
