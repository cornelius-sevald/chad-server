{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
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

type User = (Int, String)

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
    users <- newMVar []
    mainLoop sock chan users dbConn

mainLoop :: Socket -> Chan Msg -> MVar [User] -> DB.Connection -> IO ()
mainLoop sock chan users dbConn = do
    conn <- accept sock                      -- accept a connection and handle it
    forkIO $ runConn conn chan users dbConn  -- run our server's logic
    mainLoop sock chan users dbConn          -- repeat

runConn :: (Socket, SockAddr) -> Chan Msg -> MVar [User] -> DB.Connection -> IO ()
runConn (sock, _) chan users dbConn = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    let runSession' = runSession hdl chan users
    handle (\(SomeException _) -> return ()) $ do
        los  <- loginOrSignUp hdl
        user <- if los then loginUser hdl users dbConn
                       else createUser hdl users dbConn
        runSession' user
        removeLogoutUser user users -- log out the user

    hClose hdl                      -- close the handle

runSession :: Handle -> Chan Msg -> MVar [User] -> User -> IO ()
runSession hdl chan users (userID, name) = do
    let broadcast msg = getTimeStr >>= \ts ->
            writeChan chan (userID, "[" ++ ts ++ "] " ++ msg)

    broadcast $ "--> " ++ name ++ " entered the chat."
    hPutStrLn hdl $ "Welcome " ++ name ++ "!"
    hPutStrLn hdl   "Type ':help' for help."

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
          ":help" -> hPutStrLn hdl "Type ':quit' to quit and ':here' to list\
                                  \ who is online." >> loop
          ":here" -> do
              online <- readMVar users
              let printUser (_, name) = hPutStrLn hdl $ " - " ++ name
              mapM_ printUser online >> loop
          ":quit" -> hPutStrLn hdl "Bye!"
          -- otherwise, continue looping.
          _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread readThread                   -- kill the thread
    broadcast $ "<-- " ++ name ++ " left."  -- send final broadcast

-- Ask the user to login or to sign up.
loginOrSignUp :: Handle -> IO Bool
loginOrSignUp hdl =
    let askUser = do
            hPutStr hdl "Log in or sign up? [L/s] "
            T.toUpper . T.strip <$> TIO.hGetLine hdl
        evalLoop = do
            ans <- askUser
            case ans of
              ""  -> return True
              "L" -> return True
              "S" -> return False
              _   -> hPutStrLn hdl "Invalid input" >> evalLoop
     in evalLoop

createUser :: Handle -> MVar [User] -> DB.Connection -> IO User
createUser hdl users dbConn =
    let addUser = do
            lift $ hPutStr hdl "Type a new username: "
            uname <- lift $ init <$> hGetLine hdl
            lift $ hPutStr hdl "Type a new password: "
            pwd   <- lift $ init <$> hGetLine hdl

            lift $ hPutStrLn hdl "Creating user..."
            DB.addUser dbConn (T.pack uname) (T.pack pwd)

            lift $ hPutStrLn hdl "Authenticating..."
            (DB.UserField id_ u _) <- noteT "Failed to log in." $
                DB.login dbConn (T.pack uname) (T.pack pwd)
            let user = (id_, T.unpack u)
            noteT "Failed to log in." $ addLoginUser user users
            return user
        evalLoop = do
            result <- runExceptT addUser
            case result of
              Left  e    -> hPutStrLn hdl e >> evalLoop
              Right user -> return user
     in evalLoop

loginUser :: Handle -> MVar [User] -> DB.Connection -> IO User
loginUser hdl users dbConn =
    let login = do
            lift $ hPutStr hdl "Username: "
            uname <- lift $ init <$> hGetLine hdl
            lift $ hPutStr hdl "Password: "
            pwd   <- lift $ init <$> hGetLine hdl

            lift $ hPutStrLn hdl "Authenticating..."
            (DB.UserField id_ u _) <- noteT "Invalid username or password." $
                DB.login dbConn (T.pack uname) (T.pack pwd)
            let user = (id_, T.unpack u)
            noteT "User already logged in." $ addLoginUser user users
            return user
        evalLoop = do
            result <- runExceptT login
            case result of
              Left  e    -> hPutStrLn hdl e >> evalLoop
              Right user -> return user
     in evalLoop

-- Add a user to the list of logged in users,
-- unless the user is already logged in.
addLoginUser :: User -> MVar [User] -> MaybeT IO ()
addLoginUser user@(id_, _) users = do
    -- Check if the user is already logged in by ID
    alreadyLoggedIn <- lift $ elem id_ . map fst <$> readMVar users
    let addFunc = (:) user
    if alreadyLoggedIn
        then nothing
        else lift $ modifyMVar_ users (return . addFunc)

-- Remove a user of the list of logged in users.
removeLogoutUser :: User -> MVar [User] -> IO ()
removeLogoutUser (id_, _) users = do
    let removeFunc = filter (\a -> fst a /= id_)
    modifyMVar_ users (return . removeFunc)

getTimeStr :: IO String
getTimeStr = formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
