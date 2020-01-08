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

data LRUCache k v = LRUCache { capacity :: Int
                             , cache :: [(k, v)] } deriving (Eq, Show, Read)

view :: (Eq k) => k -> LRUCache k v -> (Maybe v, LRUCache k v)
view k (LRUCache n cache) = (fmap snd $ find ((== k) . fst) cache, LRUCache n $ k <|- cache)
  where (<|-) :: (Eq k) => k -> [(k, v)] -> [(k, v)]
        k' <|- xs = case findIndex ((== k') . fst) xs of
                      Nothing -> xs
                      Just i -> xs !! i : take i xs <> drop (succ i) xs

push :: (k, v) -> LRUCache k v -> LRUCache k v
push e (LRUCache n cache) = LRUCache n . take n . (e:) $ cache

empty :: Int -> Maybe (LRUCache k v)
empty n
  | n <= 0 = Nothing
  | otherwise = Just $ LRUCache n []

clear :: LRUCache k v -> LRUCache k v
clear (LRUCache n _) = LRUCache n []

size :: LRUCache k v -> Int
size = length . cache
