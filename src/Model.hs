{-# LANGUAGE OverloadedStrings #-}

module Model (Tag(..), Category(..), Record(..)) where

import Data.Hourglass
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import Data.Text
-- newtype?
newtype Tag = Tag String deriving (Show, Eq, Ord)

newtype Category = Category String deriving (Show, Eq, Ord)


instance FromRow Category where
  fromRow = Category <$> field

instance ToField Category where
  toField (Category x) = toField (pack x)

-- id category [tag] amount comment date
-- FIXME: not happy about deriving here, should be custom instance with id?
data Record = Record Int Category [Tag] Float (Maybe String) DateTime deriving (Show, Eq, Ord)
