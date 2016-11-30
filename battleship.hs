-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

-- import battleNet
import Control.Monad


p_board = [[(0,1),(6,7)],[(6,8),(0,0)]]             

check_guess :: (Int, Int) -> [[(Int, Int)]] -> Bool
check_guess coord lst
    | coord `elem` concat lst = True
    | otherwise = False
    
--update_board :: [(Int, Int)] -> [(Int, Int)]
    
-- guess: gets user's guess, send it, update board (hit/miss), signal end of turn

guess = do
    putStrLn "Please guess"
    g <- getLine
    putStrLn ("You guessed " ++ g)
    let input = read g
    let ans = check_guess input p_board
    return ans
    
    


    
--p_board :: [(Int, Int)]
-- generating boats eventually



main = forever $ do

    g <- guess
    guess


    