import System.Environment 

import Lexer (testLexer)
import Parser (testParser)


main :: IO ()
main = do
    args <- getArgs
    case args of
        a:as -> case a of
                    "--test" -> runTests as
                    _        -> putStrLn flagsError
        _   -> putStrLn flagsError


runTests :: [String] -> IO ()
runTests testFlags =
    case testFlags of
        f:fs -> case f of
                    "lexer"  ->
                        case fs of
                            [directory] -> testLexer directory
                            _           -> putStrLn flagsError
                    "parser" ->
                        case fs of
                            [directory] -> testParser directory
                            _          -> putStrLn flagsError
                    _        -> putStrLn $ f ++ " not implemented"

  
flagsError :: String
flagsError = "Use: ./Main --test [lexer|parser] [test directory]\n"
