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
    tests <- listDirectory "tests/tokens"
    mapM_ runTest tests


runTest :: String -> IO ()
runTest inputFile = do
    putStrLn $ id inputFile
    text <- readFile $ "tests/tokens/" ++ inputFile
    putStr "  "
    print text
    putStr "  "  -- does not print nicely for errors
    case Parsec.parse lex inputFile text of
        Left err  -> print err
        Right x   -> print x



-- TOKENS


data Token = Keyword String
           | UIntLit Int
           | SIntLit Int
           | StringLit String
           | Id String
           | OpenParen
           | CloseParen


instance Show Token where
    show (Keyword kw) = id kw
    show (UIntLit n) = show n
    show (SIntLit n) = show n
    show (StringLit str) = show str
    show (Id ident) = id ident
    show OpenParen = show '('
    show CloseParen = show ')'



-- LEXER


lex :: Parsec.Parsec String () [Token]
lex = do
    Parsec.manyTill token Parsec.eof


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
        ]
    Parsec.spaces
    return token



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
    elem <- escapeSequence <|> stringChar
    return elem


-- TODO: Add raw bytes and additional unicode characters
escapeSequence :: Parsec.Parsec String () String
escapeSequence = do
    escape <- Parsec.char '\\'
    char <- Parsec.oneOf "tnr\"`\\"
    return [escape, char]


-- TODO: Prohibit U+7F and <= U+20?
stringChar :: Parsec.Parsec String () String
stringChar = do
    char <- Parsec.noneOf "\\\""
    return [char]


betweenQuotes :: Parsec.Parsec String () String -> Parsec.Parsec String () String
betweenQuotes =
    Parsec.between (Parsec.char '"') (Parsec.char '"')



-- IDENTIFIERS


identifier :: Parsec.Parsec String () Token
identifier = do
    marker <- Parsec.char '$'
    name <- Parsec.many1 $ Parsec.satisfy Char.isAscii
    return (Id (marker:name))



-- PARENS


openParen :: Parsec.Parsec String () Token
openParen = do
    Parsec.char '('
    return OpenParen


closeParen :: Parsec.Parsec String () Token
closeParen = do
    Parsec.char ')'
    return CloseParen



-- KEYWORDS


keyword :: Parsec.Parsec String () Token
keyword = do
    kw <- Parsec.choice $ map (Parsec.try . Parsec.string) Keywords.keywords
    return (Keyword kw)
