-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

import BattleNet
import Data.IP


battleship:: Int -> [Int] -> Int -> IO ()
battleship ownPort destAddr destPort = engageBattleNet ownPort (toHostAddress (toIPv4 destAddr)) destPort

