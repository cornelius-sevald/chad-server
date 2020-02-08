{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString (recv, send)

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
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "hello :~|\n"
    close sock
