module BattleNet where

import Network.Socket
import System.IO
import Data.List.Split

sendGuess :: Socket -> (Int, Int) -> IO ()
sendGuess(sock, point) = 
    send sock (show point.fst ++ "," ++ show point.snd)

getGuess :: Socket -> IO (Int, Int)
getGuess(sock) = do
    rcvData <- splitOn "," (recv sock 256)
    return (read rcvData.fst, read rcvData.fst)

sendResultGuess :: Socket -> Bool -> Bool -> IO ()
sendResultGuess(sock, hit, win) = 
    send sock (show hit ++ "," ++ show win)

getResultGuess :: Socket -> IO (Bool, Bool)
getResultGuess(sock) = do
    rcvData <- splitOn "," (recv sock 256)
    return (read rcvData.fst, read rcvData.fst)






engageBattleNet :: Int -> HostAddress -> Int -> IO ()
engageBattleNet(ownPort, destAddr, destPort) = do
    clientSock <- openSock(ownPort)
    serverSock <- connectSock(destAddr, destPort)
    fireAllCannons(clientSock, (0,0))
    connectionClose(clientSock, serverSock)

fireAllCannons :: Socket -> (Int, Int) -> IO ()
fireAllCannons(sock, guess) = do
    sendGuess(sock, guess)
    getResultGuess(sock)
    -- act on result
    getGuess(sock)
    -- validate guess
    sendResultGuess(sock, True, False)
    fireAllCannons(sock, (1,1))






openSock :: Int -> IO Socket 
openSock (port) = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 1
    conn <- accept sock
    putStrLn "Connected to: " ++ show conn.snd
    return sock

connectSock :: Int -> HostAddress -> IO Socket
connectSock(port, addr) = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    connect sock (SockAddrInet port addr)
    return sock

connectionClose :: Socket -> Socket -> IO()
connectionClose(clientSock, serverSock) = do
    close(clientSock)
    close(serverSock)
