module Interface (
          askForInput
        , initReadline
        , System.Console.Readline.addHistory
        ) where

-- import qualified
import System.Console.Readline
import System.Console.ANSI
import System.IO
import Data.Map(Map(..))
import qualified Data.Map as M
import Data.List
import Data.Char (isSpace)
import Parsing
import Debug.Trace
import Model
import Data.IORef
import Data.Set(Set(..))
import qualified Data.Set as Set
import Data.Map(Map(..))
import qualified Data.Map as Map




sgrCode :: [SGR] -> String
sgrCode sgrs = setSGRCode sgrs





askForInput :: IO (Maybe String)
askForInput  = readline (setSGRCode [SetColor Foreground Vivid Blue] ++ "> " ++ (setSGRCode [Reset]))

initReadline :: IORef (Maybe (Map CategoryName (Set Tag))) -> IO ()
initReadline cache = do
  setReadlineName "expenses" -- allow conditional parsing of the inputrc file
  -- setCompletionAppendCharacter Nothing
  -- setCompletionEntryFunction ""
  -- completeInternal

  setAttemptedCompletionFunction $ Just (addRecordCompletion cache)
  -- setEventHook $ Just colorHook


bl = (setSGRCode [SetColor Foreground Vivid Blue])
blend = (setSGRCode [Reset])

data CompletionType = CCategory | CTag | CCommand

trim :: String -> String -- FIXME: very inefficient, use Data.Text ?
trim = f . f
   where f = reverse . dropWhile isSpace


split :: String -> [String] -- FIXME: also not very efficient, wrong when first letter is space
split [] = []
split s = let
            (h, rest) = span (not . isSpace) s
            rst' = if null rest then rest else tail rest
          in h : (split rst')



-- good idea, I think I can write my own completion routine given readline functions
-- or just don't use readline
addRecordCompletion ::  IORef (Maybe (Map CategoryName (Set Tag))) -> String -> Int ->Int -> IO(Maybe (String, [String]))
addRecordCompletion cache text start end = do
      -- trace ("text: " ++ text ++
      --         " \nstart: " ++ show start ++
      --         "\nend: " ++ show end )
      (Just categories) <- readIORef cache
      setAttemptedCompletionOver True

      l <- getLineBuffer
      let
        wrap Nothing = pure Nothing
        wrap (Just (x, [])) = pure (Just (x, []))
        wrap (Just (x, xs)) = do
                            b <- getLineBuffer
                            displayMatchList xs
                            setLineBuffer b
                            forcedUpdateDisplay
                            pure (Just (x, xs))
      case getCompletions (take (end) l) of
        CmdPos s -> wrap $ commandCompletion  s
        CatPos s -> wrap $ categoryCompletion categories  s
        TagPos c s -> wrap $ tagCompletion categories c s
        RefreshPos s -> wrap $ fooMatches s ["cache"]
        otherwise -> pure Nothing  -- FIXME: RemovePos && company


-- addRecordCompletion ::  String -> Int ->Int -> IO(Maybe (String, [String]))
-- addRecordCompletion text start end = do
--                   setAttemptedCompletionOver True
--                   l <- getLineBuffer
--                   return $ case addRecordCompletionAnalyzer l text start of
--                     Just CCategory -> categoryCompletion text
--                     Just CCommand  -> commandCompletion text
--                     Just CTag      -> tagCompletion text
--                     otherwise      -> Nothing

                   -- default filename completer
                -- rl_attempted_completion_over  disable default even if no matches

categoryCompletion ::  Map CategoryName (Set Tag) -> String -> Maybe (String, [String])
categoryCompletion categories txt = fooMatches txt ( name <$> Map.keys categories)
  where
    name (CategoryName x) = x

commandCompletion :: String -> Maybe (String, [String])
commandCompletion text = fooMatches text cmds

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- fuuuuck, rly?
tagCompletion :: Map CategoryName (Set Tag) -> String -> String -> Maybe (String, [String])
tagCompletion categories c s =  if maybeHead s == Just '['
                       then let
                               f (z, zs) = ("[ " ++ z, zs)
                            in f <$> fooMatches s tags

                       else fooMatches s  tags
     where
       tags = tagName <$> maybe [] Set.toList (Map.lookup (CategoryName c) categories)
       tagName (Tag x) = x

data Command = Command { name :: String
                       , action :: [String] -> IO ()
                       , description :: String}



cmds =  [ "+"
        , "add"
        , "-"
        , "rm"
        , "delete"
        , "show"
        , "category"
        , "next"
        , "prev"
        , "date"
        , "summary"
        , "ls"
        , "refresh"
        , "help"
        , "exit"
        , ":q"
        ]



mostCommonPrefix :: [String] -> String
mostCommonPrefix [] = ""
mostCommonPrefix xs = last $ takeWhile  (\p -> all (\x -> p `isPrefixOf` x) xs) ts
  where
    ts = inits (head xs)


-- data Matches = Matches String [String]

-- findMatches :: String -> [String] ->


-- FIXME: complete match -- don't show list suggestions if single or complete match
-- no rl_completion_type in readline, strange, so double <tab> not possible?
-- (of course could be done with custom state handler)
fooMatches :: String -> [String] -> Maybe (String, [String])
fooMatches text options = if (null xs)
                          then Nothing
                          else if length xs > 1
                               then
                                     Just (mcp, xs) -- not s but most common prefix
                               else Just (head xs, [])
  where
    xs = filter (isPrefixOf text) options
    mcp = mostCommonPrefix xs
    foundExact = any (== mcp) xs
