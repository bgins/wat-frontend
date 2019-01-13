import Numeric (readHex)
import Prelude hiding (lex)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import System.Directory



-- MAIN


main :: IO ()
main = do
  tests <- listDirectory "tests"
  mapM_ runTest tests


runTest :: String -> IO ()
runTest inputFile = do
    putStrLn $ id inputFile
    text <- readFile $ "tests/" ++ inputFile
    putStr "  "  -- does not print nicely for errors
    case Parsec.parse lex inputFile text of
        Left err  -> print err
        Right x   -> print x



-- TOKENS


data Token = UIntLit Int
           | SIntLit Int

instance Show Token where
  show (UIntLit n) = show n
  show (SIntLit n) = show n



-- LEXER


lex :: Parsec.Parsec String () [Token]
lex = do
  intLitToken <- integer
  return [intLitToken]


integer :: Parsec.Parsec String () Token
integer = do
    Parsec.char '+'
    sN <- hexnum <|> num
    return (SIntLit sN)
  <|> do
    Parsec.char '-'
    sN <- hexnum <|> num
    return (SIntLit (- sN))
  <|> do
    uN <- hexnum <|> num
    return (UIntLit uN)


hexnum :: Parsec.Parsec String () Int
hexnum = do
  Parsec.string "0x"
  n <- removeUnderscores <$> Parsec.many1 (Parsec.hexDigit <|> Parsec.char '_')
  return (fst $ head $ readHex n)


num :: Parsec.Parsec String () Int
num = do
  n <- removeUnderscores <$> Parsec.many1 (Parsec.digit <|> Parsec.char '_')
  return (read n :: Int)



-- UTILITY


removeUnderscores :: String -> String
removeUnderscores = filter (not . (`elem` "_" ))
