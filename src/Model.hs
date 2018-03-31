{-# LANGUAGE OverloadedStrings #-}

module Model (Tag(..), Category(..), Record(..), CategoryName(..), tagsToText) where

import Data.Hourglass
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text(pack, unpack, Text, splitOn)
import Data.List(intersperse)
import Control.Monad (join)
import Data.Functor.Contravariant(contramap)
import TimeUtils
-- import Data.Time.Clock.POSIX(utcTimeToInt64)

-- newtype?
newtype Tag = Tag String deriving (Show, Eq, Ord)

newtype CategoryName = CategoryName String deriving (Show, Eq, Ord)

instance FromField CategoryName where
  fromField = (CategoryName <$>) <$> (fromField)

instance ToField CategoryName where
  toField (CategoryName name) = toField name

-- FIXME: move everything to TEXT ?
data Category = Category String (Set Tag) deriving (Show, Eq, Ord)

instance FromRow Category where
  fromRow = mkCategory <$> field <*> field
    where
      mkCategory :: Text -> Text -> Category
      mkCategory name ts = Category (unpack name) tags
        where
          tags = Set.fromList ((Tag . unpack) <$> splitOn " " ts)

-- FIXME: newtype for tags or smth?
instance ToRow Category where
  toRow (Category name tags) = toRow (pack name, ts)
    where
      getTag (Tag x) = x
      ts =  pack $ join $ intersperse " " (getTag <$> (Set.toList tags))

-- id category [tag] amount comment date
-- FIXME: not happy about deriving here, should be custom instance with id?
data Record = Record (Maybe Int) CategoryName (Set Tag) Float (Maybe String) Date deriving (Show, Eq, Ord)

instance FromRow Record where
  fromRow = mkRecord <$> field <*> field <*> field <*> field <*> field <*> field
    where
      mkRecord :: (Maybe Int) -> CategoryName -> Text -> Float -> (Maybe String) -> Date -> Record
      mkRecord id_ c tagString amount comments ts =
        Record id_ c tags amount comments ts
        where
          tags = Set.fromList ((Tag . unpack) <$> splitOn " " tagString)

instance ToRow Record where
  toRow (Record id_ cat tags amount comments ts) =
    toRow (id_ ,cat, tagString, amount, comments, ts)
   where
     getTag (Tag x) = x
     tagString =  pack $ join $ intersperse " " (getTag <$> (Set.toList tags))

tagsToText :: Set Tag -> Text
tagsToText tgs = pack $ join $ intersperse " " (getTag <$> (Set.toList tgs))
  where
    getTag (Tag x) = x
