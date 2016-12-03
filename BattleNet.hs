module BattleNet where

import Network.Socket
import System.IO
import Data.List.Split

-- Helper Functions

-- required to convert to IO type
toIOStr :: String -> IO String
toIOStr str = return str

toIOChar :: Char -> IO Char
toIOChar char = return char

toIOCharArr :: [[Char]] -> [IO String]
toIOCharArr charArr = map toIOStr charArr

iOSplitOn :: [Char] -> [Char] -> [IO String]
iOSplitOn delim data_str = do
    map toIOStr (splitOn delim data_str) 

sendGuess :: Socket -> (Int, Int) -> IO Int
sendGuess sock point = do 
    data_var <- toIOStr(show (fst point) ++ "," ++ show (snd point))
    send sock data_var

getGuess :: Socket -> IO (Int, Int)
getGuess sock = do
    rcvData <- recv sock 256
    let frst_str = head(splitOn "," rcvData)
    let scnd_str = head(tail(splitOn "," rcvData))
    return (read(frst_str), read(scnd_str))

sendResultGuess :: Socket -> (Bool, Bool) -> IO Int 
sendResultGuess sock result = 
    send sock (show (fst result) ++ "," ++ show (snd result))

getResultGuess :: Socket -> IO (Bool, Bool)
getResultGuess sock = do
    rcvData <- recv sock 256
    let data_arr = splitOn "," rcvData
    let frst_str = head(data_arr)
    let scnd_str = head(tail(data_arr))
    return (read(frst_str), read(scnd_str))



-- Main Functions

engageBattleNet :: Int -> HostAddress -> Int -> IO ()
engageBattleNet ownPort destAddr destPort = do
    clientSock <- openSock ownPort
    serverSock <- connectSock destAddr destPort
    fireAllCannons clientSock (0,0)
    connectionClose clientSock serverSock

fireAllCannons :: Socket -> (Int, Int) -> IO ()
fireAllCannons sock guess = do
    -- our turn --
    let bytesSent = sendGuess sock guess
    let prt_str = "Guessed: " ++ show (fst guess) ++ show (snd guess)
    putStrLn prt_str
    result <- getResultGuess sock
    let prt_str = "Got back - Hit: " ++ show (fst result) ++ " Win: " ++ show (snd result)
    putStrLn prt_str
    -- TODO: act on result
    
    -- Their Turn --
    guess <- getGuess sock
    let prt_str = "They Guessed X: " ++ show (fst guess) ++ " Y: " ++ show (snd guess)
    putStrLn prt_str
    -- TODO: validate guess
    let our_result = (True, False)
    let prt_str = "Sending our result of Hit: " ++ show (fst our_result) ++ " Win: " ++ show (snd our_result)
    putStrLn prt_str
    sendResultGuess sock our_result
    fireAllCannons sock (1,1)



-- Connection Functions

openSock :: Int -> IO Socket 
openSock port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    let portNum = fromInteger(toInteger port) :: PortNumber
    bind sock (SockAddrInet portNum iNADDR_ANY)
    listen sock 1
    conn <- accept sock
    let prt_str = "Connected to: " ++ show (snd conn)
    putStrLn prt_str
    return (fst conn)

connectSock :: HostAddress -> Int-> IO Socket
connectSock addr port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    -- convert Int to Integer to PortNumber
    let portNum = fromInteger(toInteger port) :: PortNumber
    connect sock (SockAddrInet portNum addr)
    return sock

connectionClose :: Socket -> Socket -> IO()
connectionClose clientSock serverSock = do
    close(clientSock)
    close(serverSock)
