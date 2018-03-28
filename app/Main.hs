module Main where

import Lib

import System.Console.Readline
import System.Console.ANSI
import System.IO
import Data.Map(Map(..))
import qualified Data.Map as M
import Data.List
import Data.Char (isSpace)

sgrCode :: [SGR] -> String
sgrCode sgrs = setSGRCode sgrs

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
   -- _         <- sgrExample
   initReadline
   maybeLine <- readline (setSGRCode [SetColor Foreground Vivid Blue] ++ "+ " ++ (setSGRCode [Reset]))
   case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just line -> do addHistory line
                    putStrLn $ "The user input: " ++ (show line)
                    readEvalPrintLoop

 
initReadline :: IO ()
initReadline = do
  setReadlineName "Fileman" -- allow conditional parsing of the inputrc file
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



addRecordCompletionAnalyzer :: String -> String -> Int -> Maybe CompletionType
addRecordCompletionAnalyzer line text start =
       if any (\x -> x <= start) (elemIndex '"' line)
       then Nothing
       else if  not $ null (filter (\x -> elem x ['0'..'9']) line)
         then Nothing
         else if null xs
           then if head text == ':'
             then Just CCommand
             else Just CCategory
           else Just CTag
    where
      xs = take start (trim line)
      ys = split xs


addRecordCompletion ::  String -> Int ->Int -> IO(Maybe (String, [String]))
addRecordCompletion text start end = do
                  setAttemptedCompletionOver True
                  l <- getLineBuffer
                  return $ case addRecordCompletionAnalyzer l text start of
                    Just CCategory -> categoryCompletion text
                    Just CCommand  -> commandCompletion text
                    Just CTag      -> tagCompletion text
                    otherwise      -> Nothing
                   -- default filename completer
                -- rl_attempted_completion_over  disable default even if no matches

categoryCompletion :: String -> Maybe (String, [String])
categoryCompletion txt = fooMatches txt categories

commandCompletion :: String -> Maybe (String, [String])
commandCompletion = commandsMatches

tagCompletion :: String -> Maybe (String, [String])
tagCompletion txt = fooMatches txt tags

data Command = Command { name :: String
                       , action :: [String] -> IO ()
                       , description :: String}

cmds =  [ Command ":cd" undefined ""
        , Command ":ls" undefined ""
        , Command ":help" undefined ""
        ]

categories = ["foo", "bar", "baz", "foobaz"]
tags       = ["tag", "tagee", "sagee", "bagee"]

fooMatches :: String -> [String] -> Maybe (String, [String])
fooMatches text options = if (null xs)
                          then Nothing
                          else Just (head xs, xs)
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
