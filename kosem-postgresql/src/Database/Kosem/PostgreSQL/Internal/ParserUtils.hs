module Database.Kosem.PostgreSQL.Internal.ParserUtils where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (takeWhile)

type Parser = Parsec Void String

takeWhile :: (Char -> Bool) -> Parser String
takeWhile = takeWhileP Nothing

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 = takeWhile1P Nothing

pKeyword :: String -> Parser ()
pKeyword keyword = lexeme do
    void $ C.string' keyword
    notFollowedBy C.alphaNumChar

skipWhitespace :: Parser ()
skipWhitespace =
    L.space
        (void C.space1)
        (L.skipLineComment "--")
        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipWhitespace

lexemeS :: Parser a -> Parser a
lexemeS = L.lexeme spaceP

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

-- from: https://markkarpov.com/tutorial/megaparsec.html#indentationsensitive-parsing
-- unify with ParserUtils
spaceNewlineP :: Parser ()
spaceNewlineP = L.space C.space1 lineComment empty

-- from:  https://markkarpov.com/tutorial/megaparsec.html#indentationsensitive-parsing
-- unify with ParserUtils
spaceP :: Parser ()
spaceP = L.space (void $ some (C.char ' ' <|> C.char '\t')) lineComment empty

anyPred :: [a -> Bool] -> a -> Bool
anyPred ps a = any (\p -> p a) ps

dbLabel :: Parser String
dbLabel = do
    first <- satisfy isAlpha
    rest <- takeWhile (anyPred [isAlpha, isDigit, isUnderscore])
    return $ first : rest

labelP :: Parser String
labelP = lexeme dbLabel

isAlpha :: Char -> Bool
isAlpha c = charInRange '\x41' '\x5A' c || charInRange '\x61' '\x7A' c

charInRange :: Char -> Char -> Char -> Bool
charInRange low high c = low <= c && c <= high

isDigit :: Char -> Bool
isDigit = charInRange '\x30' '\x39'

isUnderscore :: Char -> Bool
isUnderscore = ('_' ==)
