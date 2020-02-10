{-# LANGUAGE OverloadedStrings #-}
module Database
    ( connect
    , initDB
    , addUser
    , login
    , AlphaNum ()
    , fromAlphaNum
    , toAlphaNum
    , toAlphaNumMaybe
    , UserField (..)
    , Connection
    ) where

import           Control.Error
import           Control.Exception.Base
import           Control.Monad.Trans.Class
import qualified Data.Text                 as T
import           Database.SQLite.Simple
import           Database.Types

dbName :: String
dbName = "chad.db"

minUnameLen :: Int
minUnameLen = 3

minPwdLen :: Int
minPwdLen = 6

connect :: IO Connection
connect = open dbName

initDB :: Connection -> IO ()
initDB conn =
    -- Create user table
    execute_ conn "CREATE TABLE IF NOT EXISTS users ( \
                   \ user_id  INTEGER PRIMARY KEY,  \
                   \ username TEXT NOT NULL UNIQUE, \
                   \ password TEXT NOT NULL         \
                \ )"

addUser :: Connection -> T.Text -> T.Text -> ExceptT String IO ()
addUser conn _uname _pwd = do
    uname <- hoistEither $ usernameFromText _uname
    pwd   <- hoistEither $ passwordFromText _pwd
    let handler e = case fromException e of
                      Just (SQLError ErrorConstraint _ _) ->
                          throwE "Username already used."
                      _                                   ->
                          throw e
    tryE (_addUser conn uname pwd) `catchE` handler

_addUser :: Connection -> AlphaNum -> AlphaNum -> IO ()
_addUser conn _uname _pwd = do
    let uname = fromAlphaNum _uname
    let pwd   = fromAlphaNum _pwd
    execute conn "INSERT INTO users (username, password) \
                \ VALUES (?,?)" (uname, pwd)

login :: Connection -> T.Text -> T.Text -> MaybeT IO UserField
login conn _uname _pwd = do
    uname <- hushT $ hoistEither $ usernameFromText _uname
    pwd   <- hushT $ hoistEither $ passwordFromText _pwd
    _login conn uname pwd

_login :: Connection -> AlphaNum -> AlphaNum -> MaybeT IO UserField
_login conn _uname _pwd = do
    let uname = fromAlphaNum _uname
    let pwd   = fromAlphaNum _pwd
    userFields <- lift $ query conn "SELECT * FROM users WHERE \
                                    \ username=? AND password=?"
                                    (uname, pwd) :: MaybeT IO [UserField]
    hoistMaybe $ headMay userFields

usernameFromText :: T.Text -> Either String AlphaNum
usernameFromText _uname
  | T.length _uname < minUnameLen = Left $ "Username must be at least " ++
      show minUnameLen ++ " characters long."
  | otherwise                     = case toAlphaNumMaybe _uname of
                                      Just uname -> Right uname
                                      Nothing    -> Left  "Username must only\
                                                    \ contain alphanumeric\
                                                    \ characters and underscore."

passwordFromText :: T.Text -> Either String AlphaNum
passwordFromText _pwd
  | T.length _pwd < minPwdLen = Left $ "Password must be at least " ++
      show minPwdLen ++ " characters long."
  | otherwise                 = case toAlphaNumMaybe _pwd of
                                  Just pwd -> Right pwd
                                  Nothing  -> Left  "Password must only\
                                              \ contain alphanumeric\
                                              \ characters and underscore."

isSQLException :: Exception e => e -> Bool
isSQLException e =
    case fromException (toException e) of
        Just SQLError {} -> False
        Nothing          -> True

tryE :: (Exception e) => IO a -> ExceptT e IO a
tryE = ExceptT . try
