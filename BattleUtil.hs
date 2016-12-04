module BattleUtil where

import System.IO
import Data.List.Split
-- Helper Functions

-- required to convert to IO type
toIOStr :: String -> IO String
toIOStr str = return str

toIOChar :: Char -> IO Char
toIOChar char = return char

toIOCharArr :: [[Char]] -> [IO String]
toIOCharArr charArr = map toIOStr charArr

iOSplitOn :: [Char] -> [Char] -> [IO String]
iOSplitOn delim data_str = do
    map toIOStr (splitOn delim data_str) 

