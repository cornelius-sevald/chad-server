{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Error
import           Control.Exception
import           Control.Monad             (unless)
import           Control.Monad.Fix         (fix)
import           Control.Monad.Trans.Class
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Time
import qualified Database                  as DB
import           Network.Socket            hiding (recv, send)
import           System.IO

type Msg = (Int, String)

type User = String

hints = defaultHints
host  = "127.0.0.1"
port  = "4242"

main :: IO ()
main = do
    dbConn <- DB.connect
    DB.initDB dbConn
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket AF_INET Stream 0
    bind sock (addrAddress addr)    -- listen on TCP port 4242.
    listen sock 3                   -- set a max of 3 queued connections
    chan <- newChan                 -- create a new FIFO channel
    _ <- forkIO $ fix $ \loop -> do -- read from the channel to prevent
        (_, _) <- readChan chan     -- memory from leaking.
        loop
    mainLoop sock chan dbConn 0

mainLoop :: Socket -> Chan Msg -> DB.Connection -> Int -> IO ()
mainLoop sock chan dbConn msgNum = do
    conn <- accept sock                      -- accept a connection and handle it
    forkIO $ runConn conn chan dbConn msgNum -- run our server's logic
    mainLoop sock chan dbConn $! msgNum + 1  -- repeat with increased msgNum

runConn :: (Socket, SockAddr) -> Chan Msg -> DB.Connection -> Int -> IO ()
runConn (sock, _) chan dbConn msgNum = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    let runSession' = runSession hdl chan msgNum
    handle (\(SomeException _) -> return ()) $ do
        los  <- loginOrSignUp hdl
        name <- if los then loginUser hdl dbConn
                       else createUser hdl dbConn
        runSession' name

    hClose hdl  -- close the handle

runSession :: Handle -> Chan Msg -> Int -> String -> IO ()
runSession hdl chan msgNum name = do
    let broadcast msg = getTimeStr >>= \ts ->
            writeChan chan (msgNum, "[" ++ ts ++ "] " ++ msg)

    broadcast $ "--> " ++ name ++ " entered the chat."
    hPutStrLn hdl $ "Welcome " ++ name ++ "!"

    commLine <- dupChan chan        -- duplicate the channel

    -- fork off a thread for reading from the duplicated channel
    readThread <- forkIO $ fix $ \loop -> do
        (nextID, line) <- readChan commLine
        -- Print the message, unless it is from this user.
        unless (nextID == msgNum) $ hPutStrLn hdl line
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

-- Ask the user to login or to sign up.
loginOrSignUp :: Handle -> IO Bool
loginOrSignUp hdl =
    let askUser = do
            hPutStr hdl "Log in or sign up? [L/s] "
            T.toUpper <$> T.strip <$> TIO.hGetLine hdl
        evalLoop = do
            ans <- askUser
            case ans of
              ""  -> return True
              "L" -> return True
              "S" -> return False
              _   -> hPutStrLn hdl "Invalid input" >> evalLoop
     in evalLoop

createUser :: Handle -> DB.Connection -> IO User
createUser hdl dbConn =
    let addUser = do
            lift $ hPutStr hdl "Type a new username: "
            uname <- lift $ init <$> hGetLine hdl
            lift $ hPutStr hdl "Type a new password: "
            pwd   <- lift $ init <$> hGetLine hdl
            DB.addUser dbConn (T.pack uname) (T.pack pwd)
            return uname
        evalLoop = do
            result <- runExceptT addUser
            case result of
              Left e  -> hPutStrLn hdl e >> evalLoop
              Right u -> return u
     in evalLoop

loginUser :: Handle -> DB.Connection -> IO User
loginUser hdl dbConn =
    let login = do
            lift $ hPutStr hdl "Username: "
            uname <- lift $ init <$> hGetLine hdl
            lift $ hPutStr hdl "Password: "
            pwd   <- lift $ init <$> hGetLine hdl
            DB.login dbConn (T.pack uname) (T.pack pwd)
        evalLoop = do
            result <- runMaybeT login
            case result of
              Nothing -> hPutStrLn hdl "Invalid username or password" >>
                  evalLoop
              Just (DB.UserField _ u _) -> return $ T.unpack u
     in evalLoop

getTimeStr :: IO String
getTimeStr = formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
