module Database.Types
    ( UserField (..)
    , AlphaNum ()
    , fromAlphaNum
    , toAlphaNum
    , toAlphaNumMaybe
    ) where

import           Data.Char                      (isAlphaNum)
import           Data.String                    (IsString (..))
import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data UserField = UserField Int T.Text T.Text
    deriving Show

instance FromRow UserField where
    fromRow = UserField <$> field <*> field <*> field

instance ToRow UserField where
    toRow (UserField id_ uname pwd) = toRow (id_, uname, pwd)

-- String type that can only hold alphanumeric characters and '_'.
-- The underlying type is a 'Text'.
newtype AlphaNum = AlphaNum
    { fromAlphaNum :: T.Text
    } deriving (Eq, Ord)

-- Convert 'Text' to 'AlphaNum',
-- discarding any non-alphanumeric characters.
toAlphaNum :: T.Text -> AlphaNum
toAlphaNum = AlphaNum . T.filter (\c -> isAlphaNum c || c == '_')

-- Safe version of toAlphaNum.
toAlphaNumMaybe :: T.Text -> Maybe AlphaNum
toAlphaNumMaybe = fmap (AlphaNum . T.pack) . mapM (\c ->
    if isAlphaNum c || c == '_'
       then Just c
       else Nothing) . T.unpack

instance Show AlphaNum where
    show = show . fromAlphaNum

instance IsString AlphaNum where
    fromString = toAlphaNum . T.pack

instance Semigroup AlphaNum where
    AlphaNum a <> AlphaNum b = AlphaNum (T.append a b)
    {-# INLINE (<>) #-}

instance Monoid AlphaNum where
    mempty = AlphaNum T.empty
    mappend = (<>)
    {-# INLINE mappend #-}
