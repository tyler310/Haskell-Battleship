-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

import BattleNet
import Data.IP


battleship:: Int -> [Int] -> Int -> Bool -> IO ()
battleship ownPort destAddr destPort starter = engageBattleNet ownPort (toHostAddress (toIPv4 destAddr)) destPort starter

