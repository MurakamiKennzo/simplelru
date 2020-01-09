module Data.Cache
  (
    Cache(..)
  ) where

import Data.LRUCache

class Cache c where
  (<|) :: (Eq k) => k -> c k v -> (Maybe v, c k v)
  (|>) :: (k, v) -> c k v -> c k v

instance Cache LRUCache where
  k <| c = view
  (|>) = push
