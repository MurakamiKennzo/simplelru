module Data.LRUCache.IO 
  (
    LRUCache
  , (<|-)
  , (-|>)
  , empty
  , clear
  , size
  , readLRU
  ) where

import qualified Data.LRUCache as LRU
import Data.IORef

type LRUCache k v = IORef (LRU.LRUCache k v)

(<|-) :: (Eq k) => k -> LRUCache k v -> IO (Maybe v)
k <|- c = do
  lrucache <- readIORef c
  let (v, newlrucache) = k `LRU.view` lrucache
  writeIORef c newlrucache
  return v

(-|>) :: (k, v) -> LRUCache k v -> IO ()
e -|> c = modifyIORef c (LRU.push e)

empty :: Int -> IO (LRUCache k v)
empty n = case LRU.empty n of
            Nothing -> error "Data.LRUCache.IO.empty: capacity <= 0"
            Just c -> newIORef c

clear :: LRUCache k v -> IO ()
clear = flip modifyIORef LRU.clear

size :: LRUCache k v -> IO Int
size = fmap LRU.size . readIORef

readLRU :: LRUCache k v -> IO (LRU.LRUCache k v)
readLRU = readIORef