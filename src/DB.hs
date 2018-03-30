{-# LANGUAGE OverloadedStrings #-}
module DB () where


import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified System.Directory as Dir
import Data.Text(pack)
import Model

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field


getOrCreateDataFolder :: IO FilePath
getOrCreateDataFolder = do
   home <- Dir.getHomeDirectory
   let dataDir = home ++ "/.expenses"
   Dir.createDirectoryIfMissing False dataDir
   pure dataDir

createSchemaIfNecessary :: Connection -> IO ()
createSchemaIfNecessary conn =
  do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS categories(category TEXT PRIMARY KEY)"

    execute_ conn (Query $ pack $
                   "CREATE TABLE IF NOT EXISTS tags (tag TEXT, category TEXT,"
                ++ "PRIMARY KEY (tag, category),"
                ++ "FOREIGN KEY(category) REFERENCES categories(category));")
    execute_ conn (Query $ pack $
                    "CREATE TABLE IF NOT EXISTS records ("
                 ++ "   id INTEGER PRIMARY KEY"
                 ++ " , category TEXT"
                 ++ " , tags TEXT"
                 ++ " , amount DOUBLE"
                 ++ " , comment TEXT"
                 ++ " , timestamp DATETIME"
                 ++ " , FOREIGN KEY(category) REFERENCES categories(category)"
                 ++ ");"
                )
--execute_ conn "CREATE TABLE IF NOT EXISTS TAGS (name TEXT PRIMARY KEY)"
-- data Record = Record Int Category [Tag] Float (Maybe String) DateTime

-- ok, so plan is dead simple:
-- cache tags / categories, hm, how?
-- everything else seems easy


main :: IO ()
main = do
  dataDir <- getOrCreateDataFolder
  let dbPath = dataDir ++ "/expenses.db"
  conn <- open dbPath
  createSchemaIfNecessary conn
  -- execute conn "INSERT INTO categories (category) VALUES (?)"
  --   (Only (Category "daz"))
  r <- query_ conn "SELECT * from categories" :: IO [Category]
  mapM_ print r
  putStrLn "OK"
  close conn
