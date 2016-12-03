-- CPSC 312 - 2016 - Games in Haskell
module Battleship where

-- import battleNet
import Control.Monad


p_board = [[(0,1),(6,7)],[(6,8),(0,0)]]
o_board = []
row = 5
col = row             

check_if_boat :: (Int, Int) -> [[(Int, Int)]] -> Bool
check_if_boat coord lst 
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
    let ans = check_if_boat input p_board -- replace this with the send guess function
    return (input, ans)
    
    
update_board g hit lst
    | hit == True = g:lst
    | otherwise = lst
    
--p_board :: [(Int, Int)]
-- generating boats eventually

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
print_row x y col boats
    | x == col = print_end("|")
    | check_if_boat (x,y) boats = print_cell("| X  ")
    | otherwise = print_cell("|    ")
    where
        print_cell str = do
            putStr(str)
            print_row (x+1) y col boats

-- print_board prints all the rows of the game grid            
print_board y 0 col boats = do
    return()
  
print_board y row col boats = do
    putStr(show y ++ " ")
    print_row 0 y col boats
    print_board (y+1) (row-1) col boats
    
-- print_screen prints the entire game boards including row/col numbers for the user
-- row: the number of rows
-- col: the number of columns
-- boats: the list of boats to display
-- example (prints empty grid)    print_screen 5 5 []
print_screen row col boats = do
    putStr("   ")
    print_col_num 0 col
    putStr("  ")
    print_border col
    print_board 0 row col boats
    putStr("  ")
    print_border col
    return()

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


-- need a start_game that does the set up and calls main   
-- has some welcome msg 
    
