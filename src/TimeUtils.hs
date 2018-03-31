module TimeUtils () where


import  Data.Hourglass
import  Data.Hourglass.Types
import  Database.SQLite.Simple.ToField
import  Database.SQLite.Simple.Ok
import  Database.SQLite.Simple.FromField
import  Database.SQLite.Simple
import qualified Data.Text as T
import Database.SQLite.Simple.Internal

-- FIXME: get rid of hourglass ??
-- | Output YYYY-MM-DD HH:MM:SS with an optional .SSS fraction part.
-- Explicit timezone attribute is not appended as per SQLite3's
-- datetime conventions.
dateTimeToString :: Date -> String
dateTimeToString = timePrint "YYYY-MM-DD" -- fuck... how do I parse this back, fuck

parseDateTime :: String -> Maybe Date
parseDateTime s = getDate <$> (timeParse "YYYY-MM-DD" s)
  where
    getDate (DateTime date _ ) = date

instance ToField Date where
    toField = SQLText . T.pack . dateTimeToString

instance FromField Date where
  fromField f@(Field (SQLText t) _) =
    case parseDateTime (T.unpack t) of
      Just t -> Ok t
      Nothing -> returnError ConversionFailed f ("couldn't parse UTCTime field")

  fromField f = returnError ConversionFailed f "expecting SQLText column type"

dateTime0 =
      DateTime { dtDate = Date { dateYear = 1970, dateMonth = December, dateDay = 31 }
               , dtTime = TimeOfDay {todHour = 23, todMin = 59, todSec = 59, todNSec = 343333 }}
