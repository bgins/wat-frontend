module Lexer where

import Control.Monad (void)
import Data.Char as Char
import Prelude hiding (lex)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import System.Directory (doesFileExist, removeFile)
import System.FilePath.Posix (takeFileName)

import Keywords

{-| This module contains parser primitives that can be used to generate a token
  stream or in a parser that generates an AST. The lex function does the former
  and Parser.hs does the latter.

  This is an implementation of the WebAssembly spec released on January 10th,
  2019. Sections of the specification are referenced with § section marks
  throughout this module.
-}

type Parser = Parsec.Parsec String ()


-- TOKENS [§6.2.2]


data Token = Keyword String
           | UIntLit Integer
           | SIntLit Integer
           | StringLit String
           | Id String
           | OpenParen
           | CloseParen
           | Reserved String deriving (Eq)



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
        , Parsec.try unsignedInteger
        , Parsec.try signedInteger
        , string
        , identifier
        , openParen
        , closeParen
        , reserved
        ]
    return token


  
-- KEYWORDS [see Keywords.hs for section references]


keyword :: Parser Token
keyword = do
    kw <- Parsec.choice $ map (Parsec.try . Parsec.string) Keywords.keywords
    return (Keyword kw)



-- INTEGER LITERALS [§6.3.1]


unsignedInteger :: Parser Token
unsignedInteger = do
    uN <- hexnum <|> num
    followedByWhitespace
    return (UIntLit uN)


signedInteger :: Parser Token
signedInteger = do
    positiveInteger <|> negativeInteger


positiveInteger :: Parser Token
positiveInteger = do
    Parsec.char '+'
    sN <- hexnum <|> num
    followedByWhitespace
    return (SIntLit sN)


negativeInteger :: Parser Token
negativeInteger = do
    Parsec.char '-'
    sN <- hexnum <|> num
    followedByWhitespace
    return (SIntLit (- sN))


hexnum :: Parser Integer
hexnum = do
    Parsec.try (Parsec.string "0x")
    hds <- digits Parsec.hexDigit
    return (fromDigits 16 hds)


num :: Parser Integer
num = do
  ds <- digits Parsec.digit
  return (fromDigits 10 ds)


digits :: Parser Char -> Parser [Integer]
digits digitParser = do
    d <- digitToInt <$> digitParser
    ds <- Parsec.many (digit digitParser)
    return (toInteger d:ds)


digit :: Parser Char -> Parser Integer
digit digitParser = do
    Parsec.optional (Parsec.char '_')
    d <- digitToInt <$> digitParser
    return (toInteger d)


fromDigits :: Integer -> [Integer] -> Integer
fromDigits base ds =
    foldl (\num d -> base*num + d) 0 ds



-- STRING LITERALS [§6.3.3]


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
    c <- Parsec.oneOf "tnr\"'\\"
    return [c]


rawByte :: Parser String
rawByte = do
    n <- Parsec.hexDigit
    m <- Parsec.hexDigit
    return [n, m]


unicodeCharacter :: Parser String
unicodeCharacter = do
    hds <- Parsec.between (Parsec.string "u{") (Parsec.string "}") (digits Parsec.hexDigit)
    cs <- unicodeHexString hds
    return ("u{" ++ cs ++ "}")


unicodeHexString :: [Integer] -> Parser String
unicodeHexString hds
    | val < 0xD800                    = return hexString
    | 0xE000 <= val && val < 0x110000 = return hexString
    | otherwise = Parsec.unexpected $ "Invalid unicode charater \"u{" ++ hexString ++ "}\" in string"
      where val = fromDigits 16 hds
            hexString = map (intToDigit . fromIntegral) hds


betweenQuotes :: Parser String -> Parser String
betweenQuotes =
    Parsec.between (Parsec.char '"') (Parsec.char '"')



-- IDENTIFIERS [§6.3.5]


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



-- PARENS [§6.2.2]


openParen :: Parser Token
openParen = do
    Parsec.char '('
    return OpenParen


closeParen :: Parser Token
closeParen = do
    Parsec.char ')'
    return CloseParen



-- RESERVED [§6.2.2]


reserved :: Parser Token
reserved = do
    r <- Parsec.many1 (Parsec.oneOf idChar)
    return (Reserved r)



-- WHITESPACE [§6.2.3]

  
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


{-| followedByWhitespace is used to disambiguate integers and reserved tokens

  When attempting to tokenize with an integer parser, we need to fail on a
  reserved token but allow whitespace (including comments) immediately following
  the integer to succeed. Since reserved tokens are a catch-all, we cannot attempt
  a longest match on them first.

  followedByWhitespace enforces whitespace after UIntLit and SIntLit tokens. The
  current version will err on reserved tokens such as [123;] since it only
  checks one semicolon out for line comments.
-}

followedByWhitespace :: Parser ()
followedByWhitespace = do
    Parsec.notFollowedBy $ Parsec.satisfy (\c -> notElem c "(); \n\t\r")



-- COMMENTS [§6.2.4]


comment :: Parser String
comment = do
    lineComment <|> blockComment


lineComment :: Parser String
lineComment = do
    Parsec.string ";;"
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


  
-- SHOW

  
instance Show Token where
    show (Keyword kw) = kw
    show (UIntLit n) = show n
    show (SIntLit n) = show n
    show (StringLit str) = show str
    show (Id ident) = ident
    show OpenParen = show '('
    show CloseParen = show ')'
    show (Reserved r) = r



-- IO


printLexOut :: FilePath -> IO ()
printLexOut target = do
    text <- readFile target
    case Parsec.parse lex target text of
        Left err  -> putStrLn $ show err
        Right out -> do
            putStrLn $ "• Token stream for " ++ (takeFileName target) ++ " •"
            putStrLn $ show out


writeLexOut :: FilePath -> FilePath -> IO ()
writeLexOut target outputDirectory = do
    clearResult errPath
    clearResult outPath
    text <- readFile target
    case Parsec.parse lex target text of
        Left err  -> writeFile errPath $ show err
        Right out -> writeFile outPath $ show out
  where targetName = reverse $ drop 4 $ reverse $ takeFileName target
        result  = outputDirectory ++ targetName
        errPath = result ++ ".toks.err"
        outPath = result ++ ".toks.out"


clearResult :: FilePath -> IO ()
clearResult file = do
    fileExists <- doesFileExist file
    if fileExists then
        removeFile file
    else
        return ()
