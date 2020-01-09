# simplelru

A simple LRU cache solution.

## usage

Here is an example for using Data.LRUCache.IO: 

```hs
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.LRUCache.IO

main :: IO ()
main = do
  cache :: LRUCache Int Int <- empty 2
  (1, 1) -|> cache
  readLRU cache >>= print
  (2, 2) -|> cache
  readLRU cache >>= print
  1 <|- cache >>= print
  (3, 3) -|> cache
  readLRU cache >>= print
  2 <|- cache >>= print
  (4, 4) -|> cache
  readLRU cache >>= print
  1 <|- cache >>= print
  3 <|- cache >>= print
  4 <|- cache >>= print
```
