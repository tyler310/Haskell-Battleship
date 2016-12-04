-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

-- import battleNet
import Control.Monad
import GameBoard


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
guess = do
    putStrLn "Enter your target (row, column): "
    g <- getLine
    putStrLn ("You guessed " ++ g)
    let input = read g
    -- need to SEND the guess, remove line below
    let ans = check_if_boat input p_board -- replace this with the send guess function
    return (input, ans)
    
    
update_board g hit lst
    | hit == True = g:lst
    | otherwise = lst
    
-- print_border prints the top/bottom boards
print_border 0 = do 
    putStrLn ("+")
    return ()
    
print_border n = do
    putStr ("+----")
    print_border (n-1)

-- print_col_num prints the column numbers for the user    
print_col_num x 0 = do
    putStr("\n")
    return()
    
print_col_num x col = do
    putStr("  " ++ show x ++ "  ")
    print_col_num (x+1) (col-1)

print_end str = do
    putStrLn str
    return()
-- print_row prints a row from the gameboard, including any boats as X's
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
print_screen row col boats hit_boats = do
    putStr("   ")
    print_col_num 0 col
    putStr("  ")
    print_border col
    print_board 0 row col boats hit_boats
    putStr("  ")
    print_border col
    return()
    
check_win_lose n ob = check_win_lose_2 n ob 0
check_win_lose_2 n ob tot
    | n > 1 = (check_win_lose_2 (n-1) ob (tot+n))
    | n <= 1  = ((length ob) == tot)


-- check_lose
    
start_game = do
    putStrLn("Your grid (B = boat, X = hit): ")
    a <-  create_all_ships max_ship_size max_row max_col []
    putStrLn(show a)
    main a [] [] []
    
    
main gb ob hit_boats guesses = do
    -- Show your game board + your progress
    putStrLn("YOU: ")
    print_screen max_row max_col gb hit_boats
    putStrLn("OPPONENT: ")
    print_screen max_row max_col ob ob


    -- Guess
    (g, ans) <- guess
    -- Update the o_board depending on the result
    let new_guesses = (g : guesses)
    let new_o_board = update_board g ans ob
    
    -- Accept the guess and check if it's right using check_if_boat
    -- add it to the hit_boats using update_board
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
                main gb new_o_board hit_boats new_guesses


    
