module Lexer where

import Control.Monad (void)
import Data.Char as Char
import Numeric (readHex)
import Prelude hiding (lex)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import System.Directory

import Keywords


-- MAIN


main :: IO ()
main = do
    tests <- listDirectory "tests/wat"
    mapM_ runTest tests


runTest :: String -> IO ()
runTest inputFile = do
    putStrLn $ id inputFile
    text <- readFile $ "tests/wat/" ++ inputFile
    -- print text
    case Parsec.parse lex inputFile text of
        Left err  -> print err
        Right x   -> print x
    putStr "\n"



-- TOKENS


data Token = Keyword String
           | UIntLit Int
           | SIntLit Int
           | StringLit String
           | Id String
           | OpenParen
           | CloseParen
           | Reserved String


instance Show Token where
    show (Keyword kw) = kw
    show (UIntLit n) = show n
    show (SIntLit n) = show n
    show (StringLit str) = show str
    show (Id ident) = ident
    show OpenParen = show '('
    show CloseParen = show ')'
    show (Reserved r) = r



-- LEXER


lex :: Parsec.Parsec String () [Token]
lex = do
    Parsec.optional whitespace
    ts <- tokens
    Parsec.eof
    return ts


tokens :: Parsec.Parsec String () [Token]
tokens = do
    Parsec.sepEndBy token whitespace


token :: Parsec.Parsec String () Token
token = do
    token <- Parsec.choice
        [ keyword
        , unsignedInteger
        , signedInteger
        , string
        , identifier
        , openParen
        , closeParen
        , reserved
        ]
    return token


whitespace :: Parsec.Parsec String () ()
whitespace = do
    Parsec.many $ space <|> format <|> Parsec.try comment
    return ()


space :: Parsec.Parsec String () String
space = do
    c <- Parsec.char ' '
    return [c]


format :: Parsec.Parsec String () String
format = do
    c <- Parsec.oneOf "\t\n\r"
    return [c]


-- KEYWORDS


keyword :: Parsec.Parsec String () Token
keyword = do
    kw <- Parsec.choice $ map (Parsec.try . Parsec.string) Keywords.keywords
    return (Keyword kw)



-- INTEGER LITERALS


unsignedInteger :: Parsec.Parsec String () Token
unsignedInteger = do
    uN <- hexnum <|> num
    return (UIntLit uN)


signedInteger :: Parsec.Parsec String () Token
signedInteger = do
    positiveInteger <|> negativeInteger


positiveInteger :: Parsec.Parsec String () Token
positiveInteger = do
    Parsec.char '+'
    sN <- hexnum <|> num
    return (SIntLit sN)


negativeInteger :: Parsec.Parsec String () Token
negativeInteger = do
    Parsec.char '-'
    sN <- hexnum <|> num
    return (SIntLit (- sN))


hexnum :: Parsec.Parsec String () Int
hexnum = do
    Parsec.try (Parsec.string "0x")
    hds <- digits Parsec.hexDigit
    return (fromDigits 16 hds)


num :: Parsec.Parsec String () Int
num = do
  ds <- digits Parsec.digit
  return (fromDigits 10 ds)


digits :: Parsec.Parsec String () Char -> Parsec.Parsec String () [Int]
digits digitParser = do
    d <- digitToInt <$> digitParser
    ds <- Parsec.many (digit digitParser)
    return (d:ds)


digit :: Parsec.Parsec String () Char -> Parsec.Parsec String () Int
digit digitParser = do
    Parsec.optional (Parsec.char '_')
    d <- digitToInt <$> digitParser
    return d


fromDigits :: Int -> [Int] -> Int
fromDigits base ds =
    foldl (\num d -> base*num + d) 0 ds



-- STRING LITERALS


string :: Parsec.Parsec String () Token
string = do
    str <- betweenQuotes (concat <$> Parsec.many stringElement)
    return (StringLit str)


stringElement :: Parsec.Parsec String () String
stringElement = do
    elem <- stringCharacter <|> escapeSequence
    return elem


stringCharacter :: Parsec.Parsec String () String
stringCharacter = do
    char <- Parsec.satisfy (\c -> c /= '\\' && c /= '\"' && not (isControl c))
    return [char]


escapeSequence :: Parsec.Parsec String () String
escapeSequence = do
    escape <- Parsec.char '\\'
    cs <- Parsec.choice
        [ escapeCharacter
        , rawByte
        , unicodeCharacter
        ]
    return (escape:cs)


escapeCharacter :: Parsec.Parsec String () String
escapeCharacter = do
    c <- Parsec.oneOf "tnr\"`\\"
    return [c]


rawByte :: Parsec.Parsec String () String
rawByte = do
    n <- Parsec.hexDigit
    m <- Parsec.hexDigit
    Parsec.lookAhead (Parsec.oneOf "\" ")
    return [n, m]


unicodeCharacter :: Parsec.Parsec String () String
unicodeCharacter = do
    hds <- Parsec.between (Parsec.string "u{") (Parsec.string "}") (digits Parsec.hexDigit)
    cs <- unicodeHexString hds
    return ("u{" ++ cs ++ "}")


unicodeHexString :: [Int] -> Parsec.Parsec String () String
unicodeHexString hds
    | val < 55296                   = return hexString   -- n < 0xD800
    | 57344 <= val && val < 1114112 = return hexString   -- 0xE000 <= n < 0x110000
    | otherwise = Parsec.unexpected $ "Invalid unicode charater \"u{" ++ hexString ++ "}\" in string"
      where val = fromDigits 16 hds
            hexString = map intToDigit hds


betweenQuotes :: Parsec.Parsec String () String -> Parsec.Parsec String () String
betweenQuotes =
    Parsec.between (Parsec.char '"') (Parsec.char '"')



-- IDENTIFIERS


identifier :: Parsec.Parsec String () Token
identifier = do
    marker <- Parsec.char '$'
    name <- Parsec.many1 (Parsec.oneOf idChar)
    return (Id (marker:name))


idChar :: String
idChar =
    concat
        [ ['0'..'9']
        , ['A'..'Z']
        , ['a'..'z']
        , [ '!', '#', '$', '%', '&', '′', '*', '+', '-', '.', '/'
          , ':', '<', '=', '>', '?', '@', '\\', '^', '_', '`', '|', '~']
        ]


-- PARENS


openParen :: Parsec.Parsec String () Token
openParen = do
    Parsec.char '('
    return OpenParen


closeParen :: Parsec.Parsec String () Token
closeParen = do
    Parsec.char ')'
    return CloseParen



-- RESERVED


reserved :: Parsec.Parsec String () Token
reserved = do
    r <- Parsec.many1 (Parsec.oneOf idChar)
    return (Reserved r)



-- COMMENTS


comment :: Parsec.Parsec String () String
comment = do
    lineComment <|> blockComment


lineComment :: Parsec.Parsec String () String
lineComment = do
    Parsec.string ";; "
    lc <- Parsec.manyTill Parsec.anyChar eol
    return lc


eol :: Parsec.Parsec String () ()
eol = do
     void Parsec.newline <|> Parsec.eof


blockComment :: Parsec.Parsec String () String
blockComment = do
    bc <- inBlockComment (concat <$> Parsec.many (Parsec.try blockComment <|> blockChar))
    return bc


blockChar :: Parsec.Parsec String () String
blockChar = do
    c <- Parsec.choice
        [ Parsec.try (Parsec.noneOf ";)")
        , Parsec.try blockCharSemi
        , Parsec.try blockCharOpenParen
        ]
    return [c]


blockCharSemi :: Parsec.Parsec String () Char
blockCharSemi = do
    Parsec.char ';'
    Parsec.notFollowedBy (Parsec.char ')')
    return ';'


blockCharOpenParen :: Parsec.Parsec String () Char
blockCharOpenParen = do
    Parsec.char '('
    Parsec.notFollowedBy (Parsec.char ';')
    return '('


inBlockComment :: Parsec.Parsec String () String -> Parsec.Parsec String () String
inBlockComment = do
    Parsec.between (Parsec.string "(;") (Parsec.string ";)")
