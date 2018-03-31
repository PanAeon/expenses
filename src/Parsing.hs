{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module Parsing(
  parseInput,
  getCompletions,
  UserRequest(..),
  CompletionRequest(..)) where

import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
-- import Text.Megaparsec.String
-- import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Arrow(left)

-- space -- skip

-- space1 :: (MonadParsec e s m, Token s ~ Char) => m ()
-- space1 = void $ takeWhile1P (Just "white space") isSpace

type Parser = Parsec Void String


data UserRequest =
         AddRequest String [String] Float (Maybe String)
       | DeleteItem Int
       | UpdateItem Int
       | RefreshCache
       | AddCategory String
       | RemoveCategory String
       | NextDate
       | PrevDate
       | SetDate Int (Maybe Int) (Maybe Int) -- day month year
       | ShowSummary
       | ShowHelp
       | EmptyRequest deriving Show



data CompletionRequest =
        CmdPos String
      | CatPos String
      | TagPos String String
      | RemovePos String
      | RefreshPos String

      | UndefinedPos deriving Show

-- ok, simple add parser:
-- '+'<main category' ws ('[' (sub_category *> ws) * ']')? ws <amount> ws ("comment")?
-- or 'add' *> ws *> main_category *> ws ...
-- or 'command'
-- commands: +,-,add,rm, show, summary, next, prev, date, help ...
-- ...

sc :: Parser ()
sc = space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

token' :: String -> Parser String
token' w = string w <* notFollowedBy alphaNumChar


symbol :: String -> Parser String
symbol = L.symbol sc

cmd :: Parser String
cmd =   (cmd' <|> cmd'')
  where
    cmd'  = many alphaNumChar
    cmd'' = (:"") <$> oneOf ['?', '+', '-']

cmd' :: Parser String
cmd' = many (notSpace)

notSpace :: Parser Char
notSpace = notChar ' '

-- actually not space?

tag :: Parser String
tag = many (alphaNumChar <|> (oneOf "_-"))

tag1 :: Parser String
tag1 = some (alphaNumChar <|> (oneOf "_-"))


amount :: Parser Float
amount = try (L.float) <|>  (fromIntegral <$> L.decimal)
-- floatP :: Parser Float


-- ignore :: Parser a -> Parser CompletionRequest
-- ignore p = EmptyPos


-- tag :: Parser String
-- tag = (lexeme . try) (p >>= check)
--   where
--     p       = (:) <$> letterChar <*> many alphaNumChar
--     check x = if x `elem` rws
--                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
--                 else return x

infixl 4 ?>>, ??>>, ?>>=

(?>>) :: Parser a -> Parser a -> Parser a
(?>>) a b = a >>= (\x -> b <|> (pure x))

(??>>) :: Parser a -> Parser (Maybe a) -> Parser a
(??>>) a b = do
             x <- a
             y <- b
             pure $ maybe x id y
  -- a >>= (\x -> b <|> (pure x))

(?>>=) :: Parser a -> (a -> Parser (Maybe a)) -> Parser a
(?>>=) a f = do
              x <- a
              y <- f x
              pure $ maybe x id y


commentP :: Parser String
commentP =  (char '"' >> manyTill L.charLiteral (char '"'))

getCompletions :: String -> CompletionRequest
getCompletions = maybe UndefinedPos id . (parseMaybe completionParser)

parseInput :: String -> Either String UserRequest
parseInput s = left parseErrorPretty (parse commandParser "" s)

-- show errors or not? probably not
completionParser :: Parser CompletionRequest
completionParser = space *> choice [

      try (string "+" *> sc) *> addCompletions
    , rword "add" *> addCompletions
    , string "-" *> space *> removeCompletions
    , rword "rm" *> removeCompletions
    , rword "show" *> showCompletions
    , rword "next" *> nextCompletions
    , rword "prev" *> prevCompletions
    , rword "date" *> dateCompletions
    , rword "refresh" *> refreshCompletions
    , rword "help" *> helpCompletions
    , CmdPos <$> (cmd' <* eof)
    ]

-- TODO: insert/rm category, tag
-- fancy summary with diff/int, month-to-month stats


addCompletions :: Parser CompletionRequest
addCompletions =
    CatPos <$> ( tag  )  ?>>= (maybeTags) <* space <* eof
  where
    tags c = (TagPos c <$> tag) ?>> ( tagEnd <|> (sc *> tags c))
    tagEnd = UndefinedPos <$  char ']'
    maybeTags (CatPos c) =   optional (
     ( (TagPos c "" ) <$ (space *> char '[' <* space))
      ?>> tags c -- TODO: think something smarter, esp. ?>>=
      )


removeCompletions :: Parser CompletionRequest
removeCompletions = RemovePos <$> many digitChar <* space <* eof

refreshCompletions :: Parser CompletionRequest
refreshCompletions = RefreshPos <$> (many digitChar <* space <* eof)

showCompletions :: Parser CompletionRequest
showCompletions = pure UndefinedPos -- TODO:

nextCompletions :: Parser CompletionRequest
nextCompletions = pure UndefinedPos
prevCompletions :: Parser CompletionRequest
prevCompletions = pure UndefinedPos

dateCompletions :: Parser CompletionRequest
dateCompletions = pure UndefinedPos -- TODO: date!

helpCompletions :: Parser CompletionRequest
helpCompletions = pure UndefinedPos


--------------------------------------------------
--- still not very good, but I'm a bit fed up with all this cruft,
-- as always promise myself to revisit sometime and clean-up this mess

commandParser :: Parser UserRequest
commandParser = space *> choice [
     EmptyRequest <$ eof
   , (string "+" *> space1 *> addParser)
   , rword "add" *> addParser
   , ShowSummary <$ (token' "ls" *> space *> eof)
   , ShowSummary <$ (token' "summary" *> space *> eof)
   , rword "-"  *> rmParser
   , rword "rm" *> rmParser
   , rword "update" *> updateParser
   -- , rword "show" *> undefined
   , NextDate <$ (token' "next" <* space <* eof)
   , PrevDate <$ (token' "prev" <* space <* eof)
   , rword "date" *> dateParser
   , rword "category" *> categoryParser
   , RefreshCache <$ (rword "refresh" *> token' "cache" *> space *> eof)
   , ShowHelp <$ (token' "help" *> space *> eof)
    ]

-- not command, but +/add command parser ..
addParser :: Parser UserRequest
addParser = do
                category <- lexeme tag1
                xs <- filter (not . null) <$> maybe [] id <$> optional (
                        char '[' *> space *>
                        (tag `sepBy` space1)
                        <* space <* char ']'
                      )
                space
                a <-  L.float
                space
                comments <- optional (commentP)
                space
                eof
                pure $ AddRequest category  xs a comments

categoryParser :: Parser UserRequest
categoryParser = do
                  subCommand <- choice [ "add" <$ rword "add", "rm" <$ rword "rm"]
                  name <- tag1
                  case subCommand of
                    "add" -> pure $ AddCategory name
                    "rm" -> undefined

rmParser :: Parser UserRequest
rmParser = DeleteItem <$> (L.decimal <* space <* eof)

updateParser:: Parser UserRequest
updateParser = UpdateItem <$> (L.decimal <* space <* eof)

dateParser :: Parser UserRequest -- FIXME: validation, partial date
dateParser =
  do
    day <- L.decimal
    string "."
    month <- L.decimal
    string "."
    year <- L.decimal
    space
    eof
    pure $ SetDate day (Just month) (Just year)

-- completionParser :: Parser CompletionPos
-- completionParser = commandCP *> categoryStartCP *> tagOpenCP *> tagStartCP *> amountStartCP *> commentCP
--
-- commandCP :: Parser CompletionPos
-- commandCP  = CommandStart <$> symbol "command"
-- categoryStartCP :: Parser CompletionPos
-- categoryStartCP = CategoryStart <$> symbol "category"
-- tagOpenCP :: Parser CompletionPos
-- tagOpenCP  = (\_ -> TagOpen)<$> symbol "["
-- tagStartCP :: Parser CompletionPos
-- tagStartCP  = TagStart <$> symbol "tag"
-- amountStartCP :: Parser CompletionPos
-- amountStartCP  = (\_ -> AmountStart) <$> symbol "tag"
-- commentCP :: Parser CompletionPos
-- commentCP = (\_ -> CommentStart) <$> symbol "tag"

{-
-- parseTest lambdaParser "λn.λf.λx.f (n f x)"
--------------------------------------------------------------------------------
addS =  "\\n.\\f.\\x.f (n f x)"
-- runState (assignLabels addL) 0
stupdidParser x = maybe (error "can not parse") id  $ parseMaybe  lambdaParser x

-}
