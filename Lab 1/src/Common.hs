module Common where

import Database.HDBC

class Common e where
  convert :: [SqlValue] -> e
  getAll :: IConnection c => c -> IO [e]
  getById :: IConnection c => c -> Integer -> IO (Maybe e)
  deleteAll :: IConnection c => c -> e -> IO Bool
  deleteById :: IConnection c => c -> Integer -> e -> IO Bool
  create :: IConnection c => c -> e -> IO e