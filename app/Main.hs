module Main where

import           Control.Concurrent
import           Data.Time
import           Network.Socket     hiding (recv, send)
import           System.IO

hints = defaultHints
host  = "127.0.0.1"
port  = "4242"

main :: IO ()
main = do
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket AF_INET Stream 0
    bind sock (addrAddress addr)    -- listen on TCP port 4242.
    listen sock 3                   -- set a max of 3 queued connections
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    forkIO $ runConn conn   -- run our server's logic
    mainLoop sock           -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    t1 <- getTimeStr
    hPutStrLn hdl $ "[" ++ t1 ++ "] " ++ "hello :~|\n"
    threadDelay 5000000

    t2 <- getTimeStr
    hPutStrLn hdl $ "[" ++ t2 ++ "] " ++  "bye...\n"
    hClose hdl

getTimeStr :: IO String
getTimeStr = formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
