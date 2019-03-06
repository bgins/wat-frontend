import System.Environment 

import Data.List (isSuffixOf)
import System.Directory (listDirectory, removeFile)

import Check (check)
import Lexer (testLexer)
import Parser (testParser)


main :: IO ()
main = do
    args <- getArgs
    case args of
        a:as ->
            case a of
                "lex"   ->
                    case as of
                        [directory] -> do
                            clearResults $ directory ++ "lex/"
                            testLexer directory
                        _           -> putStrLn flagsError
                "parse" ->
                    case as of
                        [directory] -> do
                            clearResults $ directory ++ "parse/"
                            testParser directory
                        _           -> putStrLn flagsError
                "all" ->
                    case as of
                        [directory] -> do
                            clearResults $ directory ++ "lex/"
                            clearResults $ directory ++ "parse/"
                            testLexer directory
                            testParser directory
                        _           -> putStrLn flagsError
                "check" ->
                    case as of
                        [filepath] -> do
                            check filepath
                _        -> putStrLn flagsError
        _   -> putStrLn flagsError


clearResults :: FilePath -> IO ()
clearResults directory = do
    files <- listDirectory directory
    mapM_ removeFile $ map (\f -> directory ++ f)
        $ filter (\f -> isSuffixOf ".out" f || isSuffixOf ".err" f) files


flagsError :: String
flagsError = "\nUsage: ./Main lex|parse directory\n"
