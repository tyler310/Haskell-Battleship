module BattleNet where

import BattleCon
import BattleUtil

import System.IO
import Network.Socket
import Data.List.Split

import Control.Concurrent
import Control.Exception


sendGuess :: Socket -> (Int, Int) -> IO Int
sendGuess sock point = do 
    result <- try (sendGuessHelper sock point) :: IO (Either SomeException Int)
    case result of 
        Left ex -> do
            sendGuess sock point
        Right h -> do
            putStrLn (show h)
            return h

sendGuessHelper :: Socket -> (Int, Int) -> IO Int
sendGuessHelper sock point = do 
    putStrLn ("Sending guess...")
    data_var <- toIOStr(show (fst point) ++ "," ++ show (snd point))
    send sock data_var

getResultGuess :: Socket -> IO (Bool, Bool)
getResultGuess sock = do
    result <- try (getResultGuessHelper sock) :: IO (Either SomeException (Bool, Bool))
    case result of 
        Left ex -> do
            getResultGuess sock
        Right h -> do
            return h

getResultGuessHelper :: Socket -> IO (Bool, Bool)
getResultGuessHelper sock = do
    --putStrLn ("Getting guess result...")
    rcvData <- recv sock 256
    putStrLn ("Got : " ++ rcvData)
    let data_arr = splitOn "," rcvData
    let frst_str = head(data_arr)
    let scnd_str = head(tail(data_arr))
    return (read(frst_str), read(scnd_str))

getGuess :: Socket -> IO (Int, Int)
getGuess sock = do
    result <- try (getGuessHelper sock) :: IO (Either SomeException (Int, Int))
    case result of 
        Left ex -> do
            getGuess sock
        Right h -> do
            return h

getGuessHelper :: Socket -> IO (Int, Int)
getGuessHelper sock = do
    putStrLn ("Getting guess...")
    rcvData <- recv sock 256
    let frst_str = head(splitOn "," rcvData)
    let scnd_str = head(tail(splitOn "," rcvData))
    return (read(frst_str), read(scnd_str))

sendResultGuess :: Socket -> (Bool, Bool) -> IO Int 
sendResultGuess sock result_tup = do
    result <- try (sendResultGuessHelper sock result_tup) :: IO (Either SomeException (Int))
    case result of 
        Left ex -> sendResultGuess sock result_tup
        Right h -> return h

sendResultGuessHelper :: Socket -> (Bool, Bool) -> IO Int 
sendResultGuessHelper sock result = do
    --putStrLn ("Sending guess result...")

    result <- send sock (show (fst result) ++ "," ++ show (snd result))
    return result




-- Main Functions

engageBattleNet :: Int -> HostAddress -> Int -> Bool ->  IO (Socket, Socket)
engageBattleNet ownPort destAddr destPort starter = do
    port_status <- newEmptyMVar
    conn_status <- newEmptyMVar
        
    forkIO (openSock ownPort (ConSock port_status))
    clientSock <- takeMVar port_status

    forkIO (connectSock destAddr destPort (ConSock conn_status))
    
    -- now both are ready
    readSock <- takeMVar conn_status 
    writeSock <- takeMVar port_status

    -- fireAllCannons readSock writeSock (0,0) starter
    --connectionClose clientSock opponentSock
    
    return (readSock, writeSock)


-- fireAllCannons :: Socket -> Socket -> (Int, Int) -> Bool -> IO ()
-- fireAllCannons readSock writeSock guess starter = do
--     if starter
--         then myTurn readSock writeSock guess
--         else do
--             theirTurn readSock writeSock

myTurn :: Socket -> Socket -> (Int, Int) -> IO(Bool, Bool)
myTurn readSock writeSock guess = do
    -- our turn --
    bytes_sent <- sendGuess writeSock guess
    let prt_str = "Guessed: " ++ show (fst guess) ++ show (snd guess)
    putStrLn prt_str
    putStrLn "Getting Result..."
    result <- getResultGuess readSock
    let prt_str = "Got back - Hit: " ++ show (fst result) ++ " Win: " ++ show (snd result)
    putStrLn prt_str
    -- TODO: act on result
    -- theirTurn readSock writeSock
    return result
    
theirTurn :: Socket -> Socket -> IO ()
theirTurn readSock writeSock = do
    -- Their Turn --
    guess <- getGuess readSock
    let prt_str = "They Guessed X: " ++ show (fst guess) ++ " Y: " ++ show (snd guess)
    putStrLn prt_str
    -- TODO: validate guess
    let our_result = (True, False)
    let prt_str = "Sending our result of Hit: " ++ show (fst our_result) ++ " Win: " ++ show (snd our_result)
    putStrLn prt_str
    sendResultGuess writeSock our_result
    --myTurn readSock writeSock (0,1)
    return ()
