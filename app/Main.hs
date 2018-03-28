module Main where

import Lib

import System.Console.Readline
import System.Console.ANSI
import System.IO
import Data.Map(Map(..))
import qualified Data.Map as M
import Data.List
import Data.Char (isSpace)
import Parsing
import Debug.Trace

sgrCode :: [SGR] -> String
sgrCode sgrs = setSGRCode sgrs

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
   -- _         <- sgrExample
   initReadline
   -- all right : http://www.delorie.com/gnu/docs/readline/rlman_46.html
   --             http://www.delorie.com/gnu/docs/readline/rlman_47.html
   --             https://hackage.haskell.org/package/readline-1.0.3.0/docs/System-Console-Readline.html
   maybeLine <- readline (setSGRCode [SetColor Foreground Vivid Blue] ++ "> " ++ (setSGRCode [Reset]))
   case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just line -> do addHistory line
                    putStrLn $ "The user input: " ++ (show line)
                    readEvalPrintLoop


initReadline :: IO ()
initReadline = do
  setReadlineName "expenses" -- allow conditional parsing of the inputrc file
  setCompletionAppendCharacter Nothing
  setAttemptedCompletionFunction $ Just addRecordCompletion
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



-- addRecordCompletionAnalyzer :: String -> String -> Int -> Maybe CompletionType
-- addRecordCompletionAnalyzer line text start =
--        if any (\x -> x <= start) (elemIndex '"' line)
--        then Nothing
--        else if  not $ null (filter (\x -> elem x ['0'..'9']) line)
--          then Nothing
--          else if null xs
--            then if head text == ':'
--              then Just CCommand
--              else Just CCategory
--            else Just CTag
--     where
--       xs = take start (trim line)
--       ys = split xs


-- good idea, I think I can write my own completion routine given readline functions
-- or just don't use readline
addRecordCompletion ::  String -> Int ->Int -> IO(Maybe (String, [String]))
addRecordCompletion text start end = do
      -- trace ("text: " ++ text ++
      --         " \nstart: " ++ show start ++
      --         "\nend: " ++ show end )
      setAttemptedCompletionOver True
      l <- getLineBuffer
      pure $ case getCompletions (take (end) l) of
        CmdPos s -> commandCompletion  s
        CatPos s -> categoryCompletion text s
        TagPos c s -> tagCompletion s text
        otherwise -> Nothing  -- FIXME: RemovePos && company


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

categoryCompletion ::  String -> String -> Maybe (String, [String])
categoryCompletion s txt = fooMatches s txt categories

commandCompletion :: String -> Maybe (String, [String])
commandCompletion = commandsMatches

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- fuuuuck, rly?
tagCompletion :: String -> String -> Maybe (String, [String])
tagCompletion txt s =  if maybeHead s == Just '['
                       then let
                               f (z, zs) = ("[ " ++ z, zs)
                            in f <$> fooMatches s txt tags

                       else fooMatches s txt tags

data Command = Command { name :: String
                       , action :: [String] -> IO ()
                       , description :: String}

cmds =  [ Command "+" undefined ""
        , Command "add" undefined ""
        , Command "-" undefined ""
        , Command "delete" undefined ""
        , Command "show" undefined ""
        , Command "next" undefined ""
        , Command "prev" undefined ""
        , Command "date" undefined ""
        , Command "help" undefined ""
        ]

categories = ["cat", "cat1", "cattiger", "catrine"]
tags       = ["lag", "tag", "tagee", "sagee", "bagee", "tagree"]

mostCommonPrefix :: [String] -> String
mostCommonPrefix xs = last $ takeWhile  (\p -> all (\x -> p `isPrefixOf` x) xs) ts
  where
    ts = inits (head xs)



fooMatches :: String -> String -> [String] -> Maybe (String, [String])
fooMatches s text options = if (null xs)
                          then Nothing
                          else if length options > 1
                               then Just (mostCommonPrefix xs, xs) -- not s but most common prefix
                               else Just (head xs, [])
  where
    xs = filter (isPrefixOf text) options
commandsMatches :: String -> Maybe (String, [String])
commandsMatches text = if (null xs)
                       then Nothing
                       else Just (text, xs)
   where
     xs = name <$>  filter (isPrefixOf text . name) cmds

main :: IO ()
main = readEvalPrintLoop
