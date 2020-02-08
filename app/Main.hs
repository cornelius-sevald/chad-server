module Main where

import           Control.Concurrent
import           Control.Monad.Fix  (fix)
import           Data.Time
import           Network.Socket     hiding (recv, send)
import           System.IO

type Msg = String

hints = defaultHints
host  = "127.0.0.1"
port  = "4242"

main :: IO ()
main = do
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket AF_INET Stream 0
    bind sock (addrAddress addr)    -- listen on TCP port 4242.
    listen sock 3                   -- set a max of 3 queued connections
    chan <- newChan                 -- create a new FIFO channel
    mainLoop sock chan

mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    conn <- accept sock         -- accept a connection and handle it
    forkIO $ runConn conn chan  -- run our server's logic
    mainLoop sock chan          -- repeat

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
    let broadcast msg = getTimeStr >>= \ts -> writeChan chan ("[" ++ ts ++ "] " ++ msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    commLine <- dupChan chan        -- duplicate the channel

    -- fork off a thread for reading from the duplicated channel
    forkIO $ fix $ \loop -> do
        line <- readChan commLine
        hPutStrLn hdl line
        loop

    -- read lines from the socket and broadcast them to `chan`
    fix $ \loop -> do
        line <- init <$> hGetLine hdl
        broadcast line
        loop

getTimeStr :: IO String
getTimeStr = formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
