module Ships where

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
    | len > 0 = [(x,y)] : place_ship_r ((x+1),y) (len-1)
    | otherwise = []

place_ship_d (x,y) len
    | len > 0 = [(x,y)] : place_ship_d (x,(y+1)) (len-1)
    | otherwise = []

place_ship_l (x,y) len
    | len > 0 = [(x,y)] : place_ship_l ((x-1),y) (len-1)
    | otherwise = []

place_ship_u (x,y) len
    | len > 0 = [(x,y)] : place_ship_u (x,(y-1)) (len-1)
    | otherwise = []      

-- creates a list of coords of a ship, if it doesn't overlap with any existing ships
-- 1 = build ship to the right
-- 2 = down
-- 3 = left
-- 4 = up
create_ship dir (x,y) len row col boats 
        | dir == 1 = ((check_pos (x,y) dir len row col boats), [concat (place_ship_r (x,y) len)])
        | dir == 2 = ((check_pos (x,y) dir len row col boats), [concat (place_ship_d (x,y) len)])
        | dir == 3 = ((check_pos (x,y) dir len row col boats), [concat (place_ship_l (x,y) len)])
        | dir == 4 = ((check_pos (x,y) dir len row col boats), [concat (place_ship_u (x,y) len)])
        | otherwise = ((check_pos (x,y) dir len row col boats), [])

create_destroyer (bool, boat) lst
        | bool = boat
        | otherwise =  create_destroyer (bool, boat) lst
        
        
create_player_ships n lst
    | n == 4 = lst : create_destroyer (False,[]) lst : create_player_ships (n-1)
    | otherwise = []

create_player_ships = do
    -- create the destroyer (2 spaces)
    d_dir <- randomRIO(1,4) :: IO Int
    d_x <- randomRIO(0,9) :: IO Int
    d_y <- randomRIO(0,9) :: IO Int   
    let (d_success, destroyer) = create_ship d_dir (d_x, d_y) 2 10 10 []
    putStrLn(show d_success)
    putStrLn(show destroyer)
    
    -- create the submarine
    s_dir <- randomRIO(1,4) :: IO Int
    s_x <- randomRIO(0,9) :: IO Int
    s_y <- randomRIO(0,9) :: IO Int 
    let (s_success, submarine) = create_ship s_dir (s_x, s_y) 3 10 10 (concat destroyer)
    putStrLn(show s_success)
    putStrLn(show submarine)

    -- create the battleship
    b_dir <- randomRIO(1,4) :: IO Int
    b_x <- randomRIO(0,9) :: IO Int
    b_y <- randomRIO(0,9) :: IO Int 
    --let (b_success, battleship) = create_ship b_dir (b_x, b_y) 3 10 10  (submarine : destroyer)
    --putStrLn(show b_success)
    --putStrLn(show battleship)   
    
    return()




