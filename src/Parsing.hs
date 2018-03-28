{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Parsing() where

import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
-- import Text.Megaparsec.String
-- import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char.Lexer as L

-- space -- skip

-- space1 :: (MonadParsec e s m, Token s ~ Char) => m ()
-- space1 = void $ takeWhile1P (Just "white space") isSpace

type Parser = Parsec Void String

sc :: Parser ()
sc = space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

symbol :: String -> Parser String
symbol = L.symbol sc

data UserInput = UserInput String String [String] Float (Maybe String) deriving (Show)
data CompletionPos =
      CategoryStart String
    | CommandStart String
    | TagOpen
    | TagStart String
    | AmountStart
    | CommentStart
    | EmptyCompletion deriving Show

data InputPos =
        CmdPos String
      | CatPos String
      | TagOpenPos
      | TagPos String
      | AmountPos
      | CommentOpenPos
      | EmptyPos deriving Show

-- ok, simple add parser:
-- '+'<main category' ws ('[' (sub_category *> ws) * ']')? ws <amount> ws ("comment")?
-- or 'add' *> ws *> main_category *> ws ...
-- or 'command'
-- commands: +,-,add,rm, show, summary, next, prev, date, help ...
-- ...

cmd :: Parser String
cmd =   (cmd' <|> cmd'')
  where
    cmd'  = many alphaNumChar
    cmd'' = (:"") <$> oneOf ['?', '+', '-']

-- actually not space?
cat :: Parser String
cat = many (alphaNumChar <|> (oneOf "_-"))

tagOpen :: Parser InputPos
tagOpen = TagOpenPos <$ sc

tag :: Parser String
tag = many (alphaNumChar <|> (oneOf "_-"))

tag' :: Parser String
tag' = some (alphaNumChar <|> (oneOf "_-"))

amount :: Parser InputPos
amount = CommentOpenPos <$ L.float

-- floatP :: Parser Float


-- ignore :: Parser a -> Parser InputPos
-- ignore p = EmptyPos


-- tag :: Parser String
-- tag = (lexeme . try) (p >>= check)
--   where
--     p       = (:) <$> letterChar <*> many alphaNumChar
--     check x = if x `elem` rws
--                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
--                 else return x

infixl 4 ?>>, ??>>

(?>>) :: Parser a -> Parser a -> Parser a
(?>>) a b = a >>= (\x -> b <|> (pure x))

(??>>) :: Parser a -> Parser (Maybe a) -> Parser a
(??>>) a b = do
             x <- a
             y <- b
             pure $ maybe x id y
  -- a >>= (\x -> b <|> (pure x))


commentP :: Parser String
commentP =  (char '"' >> manyTill L.charLiteral (char '"'))

completionParser :: Parser InputPos
completionParser =
  (CmdPos <$> cmd)
  ?>> (sc *> (CatPos <$> cat))
  ?>> (tagOpen)
  ??>> maybeTags
  ?>> (space *> amount)
  ?>> (space *> (EmptyPos <$ commentP) <* space)
  <* eof
        where
          tags = (TagPos <$> tag) ?>> ( tagEnd <|> (sc *> tags))
          tagEnd = try (AmountPos <$ (space *> char ']'))
          maybeTags =  optional (
           ( (TagPos "" ) <$ char '[' <* space)
            ?>> tags
            )

--------------------------------------------------




-- not command, but +/add command parser ..
commandParser :: Parser UserInput
commandParser = do
                c <- cmd -- not empty!!
                space
                category <- cat
                space
                xs <- filter (not . null) <$> maybe [] id <$> optional (
                        char '[' *> space *>
                        (tag `sepBy` space1)
                        <* space <* char ']'
                      )
                space
                a <- L.float
                space
                comments <- optional (commentP)
                space
                eof
                pure $ UserInput c category  xs a comments


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
