module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad      (unless)
import           Control.Monad.Fix  (fix)
import           Data.Time
import           Network.Socket     hiding (recv, send)
import           System.IO

type Msg = (Int, String)

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
    _ <- forkIO $ fix $ \loop -> do -- read from the channel to prevent
        (_, _) <- readChan chan     -- memory from leaking.
        loop
    mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan userID = do
    conn <- accept sock               -- accept a connection and handle it
    forkIO $ runConn conn chan userID -- run our server's logic
    mainLoop sock chan $! userID + 1  -- repeat with increased user ID

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan userID = do
    let broadcast msg = getTimeStr >>= \ts ->
            writeChan chan (userID, "[" ++ ts ++ "] " ++ msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "What is your name?"
    name <- init <$> hGetLine hdl
    broadcast $ "--> " ++ name ++ " entered the chat."
    hPutStrLn hdl $ "Welcome " ++ name ++ "!"

    commLine <- dupChan chan        -- duplicate the channel

    -- fork off a thread for reading from the duplicated channel
    readThread <- forkIO $ fix $ \loop -> do
        (nextID, line) <- readChan commLine
        -- Print the message, unless it is from this user.
        unless (nextID == userID) $ hPutStrLn hdl line
        loop

    -- read lines from the socket and broadcast them to `chan`
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- init <$> hGetLine hdl
        case line of
          -- If the user wants to quit, send a message and break the loop
          "quit" -> hPutStrLn hdl "Bye!"
          -- otherwise, continue looping.
          _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread readThread                   -- kill the thread
    broadcast $ "<-- " ++ name ++ " left."  -- send final broadcast
    hClose hdl                              -- close the handle

getTimeStr :: IO String
getTimeStr = formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
