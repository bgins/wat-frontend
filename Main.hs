import System.Environment 

import Data.List (isSuffixOf)
import Lexer (testLexer)
import Parser (testParser)
import System.Directory (listDirectory, removeFile)


main :: IO ()
main = do
    args <- getArgs
    case args of
        a:as ->
            case a of
                "lex"   ->
                    case as of
                        [directory] -> do
                            clearResults directory
                            testLexer directory
                        _           -> putStrLn flagsError
                "parse" ->
                    case as of
                        [directory] -> do
                            clearResults directory
                            testParser directory
                        _           -> putStrLn flagsError

                _        -> putStrLn flagsError
        _   -> putStrLn flagsError



clearResults :: FilePath -> IO ()
clearResults directory = do
    files <- listDirectory directory 
    mapM_ removeFile $ map (\f -> directory ++ f)
        $ filter (\f -> isSuffixOf ".out" f || isSuffixOf ".err" f) files
  

flagsError :: String
flagsError = "\nUsage: ./Main lex|parse directory\n"
