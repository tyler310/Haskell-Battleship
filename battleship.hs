-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

-- import battleNet
import MagicSum

data GameState p1board p2board p1boats p2boats turn = GameState {
                    p1_board :: p1board,
                    p2_board :: p2board, -- [(1,0,h),(2,2,m)] the hits and misses
                    p1_boats :: p1_boats, -- [(1,0), (2,0)] your boats
                    p2_boats :: p2_boats
                    next_turn :: turn} deriving (Eq, Show)
                    
                    
class Battleship a player p1board p2board p1boats p2boats where
    cur_player :: a -> player
    get_p1_board :: a-> p1board
    get_p2_board :: a-> p2board
    get_p1_boats :: a -> p1boats
    get p2_boats :: a -> p2boats
    
    check_guess :: (Int, Int) -> Bool
    
    update_board :: [(Int, Int)] -> [(Int, Int)]
    
    -- guess: gets user's guess, send it, update board (hit/miss), signal end of turn
    guess :: (Int, Int) -> a
    
    
    
    

    
placeBoats :: [] -> [(Int, Int)]
