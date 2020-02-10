module Crypto where

import qualified Crypto.KDF.BCrypt  as C (hashPassword, validatePassword)
import qualified Data.ByteString    as B
import           Data.Text.Encoding (encodeUtf8)
import           Database.Types

cost :: Int
cost = 14

hashPassword :: AlphaNum -> IO B.ByteString
hashPassword _pwd = let pwd = encodeUtf8 $ fromAlphaNum _pwd
                     in C.hashPassword cost pwd

validatePassword :: AlphaNum -> B.ByteString -> Bool
validatePassword _pwd hash = let pwd = encodeUtf8 $ fromAlphaNum _pwd
                              in C.validatePassword pwd hash
