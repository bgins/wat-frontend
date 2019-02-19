module Lexer where

import Control.Monad (void)
import Data.Char as Char
import Numeric (readHex)
import Prelude hiding (lex)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import System.Directory

import Keywords


type Parser = Parsec.Parsec String ()

-- TOKENS


data Token = Keyword String
           | UIntLit Int
           | SIntLit Int
           | StringLit String
           | Id String
           | OpenParen
           | CloseParen
           | Reserved String deriving (Eq)


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


lex :: Parser [Token]
lex = do
    Parsec.optional whitespace
    ts <- tokens
    Parsec.eof
    return ts


tokens :: Parser [Token]
tokens = do
    Parsec.sepEndBy token whitespace


token :: Parser Token
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


whitespace :: Parser ()
whitespace = do
    Parsec.many $ space <|> format <|> Parsec.try comment
    return ()


space :: Parser String
space = do
    c <- Parsec.char ' '
    return [c]


format :: Parser String
format = do
    c <- Parsec.oneOf "\t\n\r"
    return [c]


-- KEYWORDS


keyword :: Parser Token
keyword = do
    kw <- Parsec.choice $ map (Parsec.try . Parsec.string) Keywords.keywords
    return (Keyword kw)



-- INTEGER LITERALS


unsignedInteger :: Parser Token
unsignedInteger = do
    uN <- hexnum <|> num
    return (UIntLit uN)


signedInteger :: Parser Token
signedInteger = do
    positiveInteger <|> negativeInteger


positiveInteger :: Parser Token
positiveInteger = do
    Parsec.char '+'
    sN <- hexnum <|> num
    return (SIntLit sN)


negativeInteger :: Parser Token
negativeInteger = do
    Parsec.char '-'
    sN <- hexnum <|> num
    return (SIntLit (- sN))


hexnum :: Parser Int
hexnum = do
    Parsec.try (Parsec.string "0x")
    hds <- digits Parsec.hexDigit
    return (fromDigits 16 hds)


num :: Parser Int
num = do
  ds <- digits Parsec.digit
  return (fromDigits 10 ds)


digits :: Parser Char -> Parser [Int]
digits digitParser = do
    d <- digitToInt <$> digitParser
    ds <- Parsec.many (digit digitParser)
    return (d:ds)


digit :: Parser Char -> Parser Int
digit digitParser = do
    Parsec.optional (Parsec.char '_')
    d <- digitToInt <$> digitParser
    return d


fromDigits :: Int -> [Int] -> Int
fromDigits base ds =
    foldl (\num d -> base*num + d) 0 ds



-- STRING LITERALS


string :: Parser Token
string = do
    str <- betweenQuotes (concat <$> Parsec.many stringElement)
    return (StringLit str)


stringElement :: Parser String
stringElement = do
    elem <- stringCharacter <|> escapeSequence
    return elem


stringCharacter :: Parser String
stringCharacter = do
    char <- Parsec.satisfy (\c -> c /= '\\' && c /= '\"' && not (isControl c))
    return [char]


escapeSequence :: Parser String
escapeSequence = do
    escape <- Parsec.char '\\'
    cs <- Parsec.choice
        [ escapeCharacter
        , rawByte
        , unicodeCharacter
        ]
    return (escape:cs)


escapeCharacter :: Parser String
escapeCharacter = do
    c <- Parsec.oneOf "tnr\"`\\"
    return [c]


rawByte :: Parser String
rawByte = do
    n <- Parsec.hexDigit
    m <- Parsec.hexDigit
    Parsec.lookAhead (Parsec.oneOf "\" ")
    return [n, m]


unicodeCharacter :: Parser String
unicodeCharacter = do
    hds <- Parsec.between (Parsec.string "u{") (Parsec.string "}") (digits Parsec.hexDigit)
    cs <- unicodeHexString hds
    return ("u{" ++ cs ++ "}")


unicodeHexString :: [Int] -> Parser String
unicodeHexString hds
    | val < 55296                   = return hexString   -- n < 0xD800
    | 57344 <= val && val < 1114112 = return hexString   -- 0xE000 <= n < 0x110000
    | otherwise = Parsec.unexpected $ "Invalid unicode charater \"u{" ++ hexString ++ "}\" in string"
      where val = fromDigits 16 hds
            hexString = map intToDigit hds


betweenQuotes :: Parser String -> Parser String
betweenQuotes =
    Parsec.between (Parsec.char '"') (Parsec.char '"')



-- IDENTIFIERS


identifier :: Parser Token
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


openParen :: Parser Token
openParen = do
    Parsec.char '('
    return OpenParen


closeParen :: Parser Token
closeParen = do
    Parsec.char ')'
    return CloseParen



-- RESERVED


reserved :: Parser Token
reserved = do
    r <- Parsec.many1 (Parsec.oneOf idChar)
    return (Reserved r)



-- COMMENTS


comment :: Parser String
comment = do
    lineComment <|> blockComment


lineComment :: Parser String
lineComment = do
    Parsec.string ";; "
    lc <- Parsec.manyTill Parsec.anyChar eol
    return lc


eol :: Parser ()
eol = do
     void Parsec.newline <|> Parsec.eof


blockComment :: Parser String
blockComment = do
    bc <- inBlockComment (concat <$> Parsec.many (Parsec.try blockComment <|> blockChar))
    return bc


blockChar :: Parser String
blockChar = do
    c <- Parsec.choice
        [ Parsec.try (Parsec.noneOf ";)")
        , Parsec.try blockCharSemi
        , Parsec.try blockCharOpenParen
        ]
    return [c]


blockCharSemi :: Parser Char
blockCharSemi = do
    Parsec.char ';'
    Parsec.notFollowedBy (Parsec.char ')')
    return ';'


blockCharOpenParen :: Parser Char
blockCharOpenParen = do
    Parsec.char '('
    Parsec.notFollowedBy (Parsec.char ';')
    return '('


inBlockComment :: Parser String -> Parser String
inBlockComment = do
    Parsec.between (Parsec.string "(;") (Parsec.string ";)")



-- TEST


testLexer :: String -> IO ()
testLexer testDirectory = do
    tests <- listDirectory testDirectory
    mapM_ (runTest testDirectory) tests


runTest :: String -> String -> IO ()
runTest testDirectory inputFile = do
    text <- readFile $ testDirectory ++ "/" ++ inputFile
    case Parsec.parse lex inputFile text of
        Left err  -> writeFile errPath $ show err
        Right out  -> writeFile outPath $ show out
  where testName = reverse $ drop 4 $ reverse inputFile
        errPath = testDirectory ++ "/" ++ testName ++ ".err"
        outPath = testDirectory ++ "/" ++ testName ++ ".out"
