module BattleCon where

import System.IO
import Network.Socket
import Control.Concurrent
import Control.Exception

newtype ConSock a = ConSock (MVar a)

openSock :: Int -> ConSock Socket -> IO()
openSock port (ConSock status) = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    let portNum = fromInteger(toInteger port) :: PortNumber
    bind sock (SockAddrInet portNum iNADDR_ANY)
    putStrLn ("Bound to port " ++ show port)
    putStrLn "Listening..."
    putMVar status sock
    listen sock 1
    conn <- accept sock
    putStrLn ("Connected to: " ++ show (snd conn))
    putMVar status (fst conn)


connectSock :: HostAddress -> Int -> ConSock Socket -> IO ()
connectSock addr port (ConSock conn_status) = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    --setSocketOption sock SendTimeOut 100
    --setSocketOption sock RecvTimeOut 100
    -- convert Int to Integer to PortNumber
    let portNum = fromInteger(toInteger port) :: PortNumber
    putStrLn ("Attempting to connect to " ++ show addr ++ " at port " ++ show port)
    connectHelper sock (SockAddrInet portNum addr) (ConSock conn_status)

connectHelper :: Socket -> SockAddr -> ConSock Socket -> IO()
connectHelper sock addr (ConSock conn_status) = do
    putStrLn ("Attempting to connect...")
    result <- try (connect sock addr) :: IO (Either SomeException ())
    case result of
        Left ex -> do
            threadDelay 20
            connectHelper sock addr (ConSock conn_status)
        Right h -> do 
            putMVar conn_status sock

connectionClose :: Socket -> Socket -> IO()
connectionClose clientSock opponentSock = do
    close(clientSock)
    close(opponentSock)
