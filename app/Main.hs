module Main where

import qualified DB as DB
import  Model
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Time.System(dateCurrent)
import Time.Types(Date(..), DateTime(..))
import Data.Hourglass(dateAddPeriod, periodDays)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef
import Interface
import Parsing
import Control.Monad(void, join)

-- FIXME: show current date
-- FIXME: put status of last operation on the left ?
main :: IO ()
main = do
   c <- DB.initConnection
   categoryCache <- newIORef (Nothing :: Maybe (Map CategoryName (Set Tag)))
   _ <- DB.getCategories c categoryCache -- inits cache
   (DateTime currentDate  _) <- dateCurrent
   selectedDate <- newIORef currentDate
   readEvalPrintLoop c categoryCache selectedDate
-- init,date,cache, db
-- run user interface


-- well, readerT? ))
readEvalPrintLoop :: DB.Connection -> IORef (Maybe (Map CategoryName (Set Tag))) ->
                   IORef Date ->          IO ()
readEvalPrintLoop conn categoryCache selectedDate = do
   initReadline categoryCache
   maybeLine <- askForInput
   case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just ":q"   -> return ()
    Just line   -> do
                     addHistory line
                     putStrLn $ "The user input: " ++ (show line)
                     -- parse and execute action ...
                     case parseInput line of
                       Left err -> putStrLn $ "Hmm: " ++ err
                       Right EmptyRequest -> pure ()
                       Right r@(AddRequest _ _ _ _) -> addRecord conn categoryCache selectedDate r
                       Right (UpdateItem rownum) -> updateItem conn categoryCache selectedDate rownum
                       Right (DeleteItem rownum) -> deleteRecord conn selectedDate rownum
                       Right r@(AddCategory name) -> addCategory conn categoryCache name
                       Right RefreshCache -> refreshCache conn categoryCache
                       Right ShowSummary -> showSummary conn categoryCache selectedDate
                       Right NextDate -> setNextDate selectedDate
                       Right PrevDate -> setPrevDate selectedDate
                       Right (SetDate day maybeMonth maybeYear) -> setDate selectedDate day maybeMonth maybeYear  -- FIXME: current
                     readEvalPrintLoop conn categoryCache selectedDate

addRecord :: DB.Connection
       -> IORef (Maybe (Map CategoryName (Set Tag)))
       -> IORef Date
       -> UserRequest
       -> IO ()
addRecord c cacheRef selectedDate (AddRequest cat tags amount comments ) =
  do
    (Just categories) <- readIORef cacheRef
    if Map.notMember (CategoryName cat) categories
    then putStrLn $ "No such category '" ++ cat ++"'"
    else do
           d <- readIORef selectedDate
           let tgs = (Set.fromList (Tag <$> tags))
               cn  = (CategoryName cat)
               r = Record Nothing cn tgs amount comments d
           DB.insertRecord c r
           DB.addTags c cacheRef cn tgs
           -- insert tags
deleteRecord :: DB.Connection -> IORef Date -> Int -> IO ()
deleteRecord c selectedDate rownum =
  do
    d <- readIORef selectedDate
    rs <- DB.getRecordsByDate c d
    let _id (Record (Just x) _ _ _ _ _) = x
        rs' = L.sortOn _id rs
    if (length rs < (rownum + 1))
    then putStrLn $ "No such rownum: " ++ show rownum
    else do
           let r = rs' !! rownum
           DB.deleteRecord c (_id r)
           putStrLn $ "Record #" ++ show rownum ++ " deleted"

updateItem :: DB.Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> IORef Date -> Int -> IO ()
updateItem c cacheRef selectedDate rownum =
  do
    d <- readIORef selectedDate
    rs <- DB.getRecordsByDate c d
    let _id (Record (Just x) _ _ _ _ _) = x
        rs' = L.sortOn _id rs
    if (length rs < (rownum + 1))
    then putStrLn $ "No such rownum: " ++ show rownum
    else do
           let r = rs' !! rownum -- FIXME: update promput (u> orange .. ++ formatRecordPlain <- to hell with complete? (easy to add, in principle)
           -- wait for user input ..., parse finish
           pure ()
           -- put rownum on screen and (wait for input, but allow update only... hm)
           -- wait ...
           -- readEvalPrintLoop c cacheRef selectedDate
           -- putStrLn $ "Record #" ++ show rownum ++ " deleted"

refreshCache :: DB.Connection -> IORef (Maybe (Map CategoryName (Set Tag))) -> IO ()
refreshCache c cache = void $ DB.doGetCategories c cache

addCategory :: DB.Connection
       -> IORef (Maybe (Map CategoryName (Set Tag)))
       -> String
       -> IO ()

addCategory c cacheRef name =
    do
      (Just m) <- readIORef cacheRef
      if (Map.member (CategoryName name ) m)
      then pure ()
      else DB.upsertCategory c cacheRef (Category name Set.empty)

showSummary :: DB.Connection
         -> IORef (Maybe (Map CategoryName (Set Tag)))
         -> IORef Date
         -> IO ()

setNextDate :: IORef Date -> IO ()
setNextDate curr = modifyIORef curr (\d -> d `dateAddPeriod` mempty { periodDays = 1})

setPrevDate :: IORef Date -> IO ()
setPrevDate curr = modifyIORef curr (\d ->  d `dateAddPeriod` mempty { periodDays = -1})

setDate :: IORef Date -> Int -> Maybe Int -> Maybe Int ->  IO ()
setDate ioRef day (Just month) (Just year)  = writeIORef ioRef date
   where
     date = Date { dateDay = day, dateMonth = toEnum (month - 1), dateYear = year}


-- assuming terminal width 120
terminalWidth = 120

showSummary conn categoryCache selectedDate =
  do
    d <- readIORef selectedDate
    rs <- DB.getRecordsByDate conn d
    let _id (Record (Just x) _ _ _ _ _) = x
        _amount (Record _ _ _ x _ _) = x
        rs' = L.sortOn _id rs
        rs'' = L.zip [0..] rs'
        total = foldl (+) 0.0 (_amount <$> rs')
    putStrLn $ L.replicate terminalWidth '*'
    putStrLn ""
    let
       a = "    Date:  " ++ showDate d
       o = 60 - length a
    putStrLn $ a ++ (L.replicate o ' ') ++ " Total: " ++ (show total)

    putStrLn ""
    putStrLn $ "  Rownum  Category     TAGS                   AMOUNT  COMMENT"
    sequence_ (printRecordSummary <$> rs'') -- TODO: beautiful border, color
    putStrLn $ (L.replicate (max 0 (5 - length rs)) '\n') -- would be 5 minimum length
    putStrLn ""
    putStrLn $ L.replicate terminalWidth '*'
    putStrLn ""

    pure ()

-- FIXME: print tags (only 5)
-- FIXME: color, borders !
printRecordSummary :: (Int, Record) -> IO ()
printRecordSummary (rownum, (Record (Just id_) (CategoryName cat) tags amount comm ts)) =
  do
    let
        id' = (show rownum)
        cat' = cat
        tags' = ""
        amount' = show amount
        comments' = maybe "" id comm
        offset n xs = L.replicate (max 0 (n - length xs)) ' '
    putStrLn $ "  " ++ id' ++ (offset 8 id')
                    ++ cat ++ (offset 12 cat)
                    ++ (offset 24 "" )
                    ++ amount' ++ (offset 8 amount')
                    ++ comments'

showDate :: Date -> String
showDate (Date y m d) = show d ++ " " ++ show m ++ " " ++ show y

formatRecordPlain :: Record -> String
formatRecordPlain (Record _ (CategoryName cat) tags amount comm ts) =
   cat ++ " " ++ printTags ++ " " ++ (show amount) ++ " " ++ printComment
   where
     getTag (Tag x) = "x"
     printTags = if length tags > 0
                 then "[" ++ join ( L.intersperse " " (getTag <$> Set.toList tags) ) ++ "]"
                 else ""
     printComment = maybe "" (\x -> '"':x ++ "\"") comm
