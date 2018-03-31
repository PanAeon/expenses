{-# LANGUAGE OverloadedStrings #-}
module DB(
     Database.SQLite.Simple.Connection
   , initConnection
   , insertRecord
   , getCategories
   , addTags
   , doGetCategories
   , upsertCategory
   , deleteCategory
   , getRecordsByDate
   , getRecordsByRange
   , deleteRecord
)  where


import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified System.Directory as Dir
import Data.Text(pack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import  Data.Hourglass.Types(Date)
import Data.IORef
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

-- data Record = Record (Maybe Int) CategoryName (Set Tag) Float (Maybe String) DateTime deriving (Show, Eq, Ord)

-- x = (Record Nothing (CategoryName "foo") (Set.fromList [Tag "a", Tag "b"]), 3.14, Just "foo", undefined)

createSchemaIfNecessary :: Connection -> IO ()
createSchemaIfNecessary conn =
  do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS categories(category TEXT PRIMARY KEY, tags TEXT)"

    -- execute_ conn (Query $ pack $
    --                "CREATE TABLE IF NOT EXISTS tags (tag TEXT, category TEXT,"
    --             ++ "PRIMARY KEY (tag, category),"
    --             ++ "FOREIGN KEY(category) REFERENCES categories(category));")
    --                  ++ " , FOREIGN KEY(category) REFERENCES categories(category)"
    execute_ conn (Query $ pack $
                    "CREATE TABLE IF NOT EXISTS records ("
                 ++ "   id INTEGER PRIMARY KEY"
                 ++ " , category TEXT"
                 ++ " , tags TEXT"
                 ++ " , amount DOUBLE"
                 ++ " , comment TEXT"
                 ++ " , timestamp DATE"
                 ++ ");"
                )
--execute_ conn "CREATE TABLE IF NOT EXISTS TAGS (name TEXT PRIMARY KEY)"
-- data Record = Record Int Category [Tag] Float (Maybe String) DateTime

getAllCategoriesQ :: String
getAllCategoriesQ = "select category from categories;"

explodeCategories :: [Category] -> Map CategoryName (Set Tag)
explodeCategories = foldl f Map.empty
   where
     f m (Category name tags) = Map.insert (CategoryName name) tags m

doGetCategories :: Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> IO (Map CategoryName (Set Tag))
doGetCategories conn cache =
  do
  r <- explodeCategories <$> (query_ conn "SELECT * from categories" :: IO [Category])
  writeIORef cache (Just r)
  pure r

getCategories :: Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> IO (Map CategoryName (Set Tag))
getCategories conn cache =
  do
    maybeSet <- readIORef cache
    maybe (doGetCategories conn cache) pure maybeSet

addTags :: Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> CategoryName -> Set Tag -> IO ()
addTags conn cache categoryName@(CategoryName name) tags =
        do
          (Just categories) <- readIORef cache
          maybe (pure ()) (updateTags categories) (Map.lookup categoryName categories)
   where
     updateTags categories xs  =
                     if tags `Set.isSubsetOf` xs
                     then pure ()
                     else do
                          let allTags = Set.union xs tags
                              c1 = Category name allTags
                          upsertCategory conn cache c1
                          -- writeIORef cache (Just (Map.insert categoryName allTags categories))

rmTags :: Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> CategoryName -> Set Tag -> IO ()
rmTags = undefined

-- FIXME: reader for connection and cache values ...
upsertCategory :: Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> Category -> IO ()
upsertCategory conn cache category@(Category name tags) =
  do
    maybeSet <- readIORef cache
    xs <- query conn "SELECT category FROM categories where category = ?" (Only name) :: IO [[String]]
    if (null xs)
    then execute conn "INSERT INTO categories (category, tags) VALUES (?, ?)" category
    else execute conn "UPDATE categories SET tags = ? where category = ?" (tagsToText tags,  name)
    modifyIORef cache (ins category)
  where
    ins category Nothing  = Nothing
    ins (Category name xs) (Just m) = (Just (Map.insert (CategoryName name) xs m))

-- shit, bloody clean-up... and constraints will fail ...
-- all right think later of something smarter...
deleteCategory :: Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> CategoryName -> IO ()
deleteCategory conn cache c@(CategoryName name) =
  do
    maybeSet <- readIORef cache
    execute conn "delete from categories where category = ?" (Only name)
    modifyIORef cache _delete
  where
    _delete Nothing  = Nothing
    _delete (Just m) = (Just (Map.delete c m))


getAllRecords :: Connection ->  IO [Record]
getAllRecords conn  =
    query_ conn "SELECT * from records" :: IO [Record]


headOption ([]) = Nothing
headOption (x:xs) = Just x

getRecordById :: Connection -> Int -> IO (Maybe Record)
getRecordById conn id_ =
   headOption <$> (query conn "SELECT * from records where id = ?"
   (Only id_) :: IO [Record])


deleteRecord :: Connection -> Int -> IO ()
deleteRecord conn id_ =
  execute conn "DELETE from records where id = ?" (Only id_)

insertRecord :: Connection -> Record -> IO ()
insertRecord c r =
  execute c "INSERT INTO records (id, category, tags, amount, comment, timestamp) VALUES (?, ?, ?, ?, ?, ?)" r

getRecordsByDate :: Connection -> Date -> IO [Record]
getRecordsByDate c d =
  query c "SELECT * from records where timestamp = ? " (Only d) :: IO [Record]

-- beginning included, end excluded
getRecordsByRange :: Connection -> Date -> Date -> IO [Record]
getRecordsByRange c b e =
   query c "SELECT * from records where timestamp >= ? and timestamp < ? " (b, e) :: IO [Record]

initConnection :: IO Connection
initConnection = do
   dataDir <- getOrCreateDataFolder
   let dbPath = dataDir ++ "/expenses.db"
   conn <- open dbPath
   createSchemaIfNecessary conn
   pure conn

-- categoryCache <- newIORef (Nothing :: Maybe (Map CategoryName (Set Tag)))

-- main :: IO ()
-- main = do
--   dataDir <- getOrCreateDataFolder
--   let dbPath = dataDir ++ "/expenses.db"
--   conn <- open dbPath
--   createSchemaIfNecessary conn
--   -- execute conn "INSERT INTO categories (category) VALUES (?)"
--   --   (Only (Category "daz"))
--   r <- query_ conn "SELECT * from categories" :: IO [Category]
--   mapM_ print r
--   putStrLn "OK"
--   close conn
