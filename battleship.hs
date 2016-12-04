-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

import BattleNet
import GameBoard
import Control.Monad
import Data.IP


p_board = [(0,1),(6,7),(6,3),(0,0),(1,1)]
o_board = []
max_col = 8
max_row = 8            
max_ship_size = 3

check_if_boat :: (Int, Int) -> [(Int, Int)] -> Bool
check_if_boat coord lst 
    | coord `elem` lst = True
    | otherwise = False
    
--update_board :: [(Int, Int)] -> [(Int, Int)]
    
-- guess: gets user's guess, send it, update board (hit/miss), signal end of turn
guess :: IO ((Int, Int), Bool)
guess = do
    -- get the player's guess
    putStrLn "Enter your target (row, column): "
    g <- getLine
    putStrLn ("You guessed " ++ g)
    let input = read g
    
    -- need to SEND the guess, remove line below
    let ans = check_if_boat input p_board -- replace this with the send guess function, ans should be the bool reply
    return (input, ans)
    
update_board :: (Int, Int) -> Bool -> [(Int, Int)] -> [(Int, Int)]
update_board g hit lst
    | hit == True = g:lst
    | otherwise = lst
    
-- print_border prints the top/bottom boards
print_border :: Int -> IO ()

print_border 0 = do 
    putStrLn ("+")
    return ()
 
print_border n = do
    putStr ("+----")
    print_border (n-1)

-- print_col_num prints the column numbers for the user    
print_col_num :: Int -> Int -> IO ()
print_col_num x 0 = do
    putStr("\n")
    return()
    
print_col_num x col = do
    putStr("  " ++ show x ++ "  ")
    print_col_num (x+1) (col-1)

print_end :: String -> IO ()    
print_end str = do
    putStrLn str
    return()
    
-- print_row prints a row from the gameboard, including any boats as B's and hits as X's
print_row :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> IO ()
print_row x y col boats hit_boats
    | x == col = print_end("|")
    | check_if_boat (x,y) hit_boats = print_cell("| X  ")
    | check_if_boat (x,y) boats = print_cell("| B  ")
    | otherwise = print_cell("|    ")
    where
        print_cell str = do
            putStr(str)
            print_row (x+1) y col boats hit_boats

-- print_board prints all the rows of the game grid       
print_board :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> IO ()  
print_board y 0 col boats hit_boats = do
    return()
  
print_board y row col boats hit_boats = do
    putStr(show y ++ " ")
    print_row 0 y col boats hit_boats
    print_board (y+1) (row-1) col boats hit_boats
    
-- print_screen prints the entire game boards including row/col numbers for the user
-- row: the number of rows
-- col: the number of columns
-- boats: the list of boats to display
-- example (prints empty grid)    print_screen 5 5 []
print_screen :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> IO ()
print_screen row col boats hit_boats = do
    putStr("   ")
    print_col_num 0 col
    putStr("  ")
    print_border col
    print_board 0 row col boats hit_boats
    putStr("  ")
    print_border col
    return()

-- checks the win condition
check_win_lose :: Int -> [(Int, Int)] -> Bool    
check_win_lose n ob = check_win_lose_2 n ob 0
check_win_lose_2 n ob tot
    | n > 1 = (check_win_lose_2 (n-1) ob (tot+n))
    | n <= 1  = ((length ob) == tot)


-- sets up the game board by placing the ships, starts the game by calling main
start_game:: Int -> [Int] -> Int -> Bool -> IO ()
start_game ownPort destAddr destPort starter = do
    opp_sock <- engageBattleNet ownPort (toHostAddress (toIPv4 destAddr)) destPort starter
    
    putStrLn("Your grid (B = boat, X = hit): ")
    a <-  create_all_ships max_ship_size max_row max_col []
    putStrLn(show a)
    main a [] [] []


-- the main game function    
main :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> IO ()     
main gb ob hit_boats guesses = do
    -- Show your game board + your progress
    putStrLn("YOU: ")
    print_screen max_row max_col gb hit_boats
    putStrLn("OPPONENT: ")
    print_screen max_row max_col ob ob

    -- TODO: possibly create a player 2 file that accepts a guess first, THEN sends the guess (but otherwise the same)
    -- Get user guess
    (g, ans) <- guess
    -- Update the o_board depending on the result
    let new_guesses = (g : guesses)
    let new_o_board = update_board g ans ob
    
    -- TODO
    -- 1) Accept the opponent guess 
    -- 2) Check if it's right using check_if_boat
    -- 3) Add it to the hit_boats using update_board (just like new_o_board above)
    
    
    
    
    
    -- we check if we won/lost, otherwise progress to the next turn
    let win = check_win_lose max_ship_size new_o_board
    let lose = check_win_lose max_ship_size hit_boats
    if win
        then do
            print_screen max_row max_col ob ob
            putStrLn("You win!")
            return()
        else if lose
            then do
                putStrLn("YOU LOSE!")
                return()
            else do  
                putStr("correct guesses ")
                putStrLn(show new_o_board)
                putStr("all guesses ")
                putStrLn(show new_guesses)
                main gb new_o_board hit_boats new_guesses -- TODO replace hit_boats with the name for the updated list




