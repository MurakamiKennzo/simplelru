{-|
Module      : Data.Cache
Copyright   : Copyright (jp) 2020-present, murakami
License     : BSD3
Maintainer  : murakami

The class Cache.
-}
module Data.Cache
  (
    Cache(..)
  ) where

import Data.LRUCache

class Cache c where
  {-# MINIMAL (<|), (|>) #-}

  -- | view value by key
  (<|) :: (Eq k) => k -> c k v -> (Maybe v, c k v)

  -- | push a cache item
  (|>) :: (k, v) -> c k v -> c k v

instance Cache LRUCache where
  (<|) = view
  (|>) = push
