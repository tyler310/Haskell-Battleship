-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

-- import battleNet
import Control.Monad


p_board = [[(0,1),(6,7)],[(6,8),(0,0)]]
o_board = []             

check_guess :: (Int, Int) -> [[(Int, Int)]] -> Bool
check_guess coord lst 
    | coord `elem` concat lst = True
    | otherwise = False
    
--update_board :: [(Int, Int)] -> [(Int, Int)]
    
-- guess: gets user's guess, send it, update board (hit/miss), signal end of turn

guess = do
    putStrLn "Enter your target (row, column): "
    g <- getLine
    putStrLn ("You guessed " ++ g)
    let input = read g
    -- need to SEND the guess, remove line below
    let ans = check_guess input p_board -- replace this with the send guess function
    return (input, ans)
    
    
update_board g hit lst
    | hit == True = g:lst
    | otherwise = lst
    
--p_board :: [(Int, Int)]
-- generating boats eventually



main = forever $ do
    -- Guess
    (g, ans) <- guess
    -- Update the o_board depending on the result
    let new_o_board = update_board g ans o_board
    let o_board = new_o_board
    
    -- Accept the guess and check if it's right using check_guess
    
    -- Send the True/False (hit/miss) back
    
    putStrLn(show o_board)
    guess


    