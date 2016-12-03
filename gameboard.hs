module GameBoard where

import System.Random
import Control.Monad




check_pos (x,y) dir len row col boats
    | dir == 1 = check_pos_r (x,y) len row col boats
    | dir == 2 = check_pos_d (x,y) len row col boats
    | dir == 3 = check_pos_l (x,y) len row col boats
    | dir == 4 = check_pos_u (x,y) len row col boats
    | otherwise = False
    where
        check_pos_r (x,y) len row col boats
            | len == 0 = True
            | (x,y) `elem` boats = False
            | x > col = False
            | x < 0 = False
            | len > 0 = check_pos_r ((x+1),y) (len-1) row col boats
        check_pos_d (x,y) len row col boats
            | len == 0 = True
            | (x,y) `elem` boats = False
            | y > row = False
            | y < 0 = False
            | len > 0 = check_pos_d (x,(y+1)) (len-1) row col boats
        check_pos_l (x,y) len row col boats
            | len == 0 = True
            | (x,y) `elem` boats = False
            | x > col = False
            | x < 0 = False
            | len > 0 = check_pos_l ((x-1),y) (len-1) row col boats
        check_pos_u (x,y) len row col boats
            | len == 0 = True 
            | (x,y) `elem` boats = False
            | y > row = False
            | y < 0 = False
            | len > 0 = check_pos_u (x,(y-1)) (len-1) row col boats       
            
place_ship_r (x,y) len
    | len > 0 = (x,y) : place_ship_r ((x+1),y) (len-1)
    | otherwise = []

place_ship_d (x,y) len
    | len > 0 = (x,y) : place_ship_d (x,(y+1)) (len-1)
    | otherwise = []

place_ship_l (x,y) len
    | len > 0 = (x,y) : place_ship_l ((x-1),y) (len-1)
    | otherwise = []

place_ship_u (x,y) len
    | len > 0 = (x,y) : place_ship_u (x,(y-1)) (len-1)
    | otherwise = []      

prepend_list [] lst = lst
prepend_list (h:t) lst = h : prepend_list t lst
    
-- creates a list of coords of a ship, if it doesn't overlap with any existing ships
-- 1 = build ship to the right
-- 2 = down
-- 3 = left
-- 4 = up
create_ship dir (x,y) len row col boats 
        | dir == 1 = prepend_list boats (place_ship_r (x,y) len)
        | dir == 2 = prepend_list boats (place_ship_d (x,y) len)
        | dir == 3 = prepend_list boats (place_ship_l (x,y) len)
        | dir == 4 = prepend_list boats (place_ship_u (x,y) len)
        | otherwise = boats 

    
create_all_ships 2 row col boats = do
    x <- randomRIO(0,9) :: IO Int
    y <- randomRIO(0,9) :: IO Int
    dir <- randomRIO(1,4) :: IO Int
    
    if check_pos (x,y) dir 2 row col boats
        then         
            return (create_ship dir (x,y) 2 row col boats)
        else
            create_all_ships 2 row col boats
    
create_all_ships n row col boats = do
    x <- randomRIO(0,(col-1)) :: IO Int
    y <- randomRIO(0,(row-1)) :: IO Int
    dir <- randomRIO(1,4) :: IO Int
    
    if (check_pos (x,y) dir n row col boats)
        then
            create_all_ships (n-1) row col (create_ship dir (x,y) n row col boats) 
        else
            create_all_ships n row col boats
       





