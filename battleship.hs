-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

-- import battleNet
import MagicSum


battleship:: Game -> Result -> (Int, AMove)
-- battleship game result   =>  (value_to_player, move)
battleship game (ContinueGame st avail)  =
      maximum [value game (game (Move amove st)) amove
               | amove <- avail]

-- value game result move = (value,move) for current player of the state after move given result
value:: Game -> Result -> AMove -> (Int, AMove)
value _  (EndOfGame val) move = (val,move)
value game res move = 
   let (val,_) = battleship game res   -- (value,move) for next player
   in (-val,move)  -- value for current player is negative of value of the other player


mm_player:: Player
mm_player game result = snd ( battleship game result)
