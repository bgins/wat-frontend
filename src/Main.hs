import System.Environment

import Data.Semigroup ((<>))
import Options.Applicative

import Check (check)
import Lexer (printLexOut, writeLexOut)
import Parser (printParseOut, writeParseOut)



-- ARGS


data Args = Args
  { action   :: String
  , target   :: String
  , maybeOut :: Maybe String }

args :: Parser Args
args = Args
      <$> argument str
          (metavar "PHASE"
         -- <> help "lex, parse, or check" )
         <> help "lex [produce a token stream], \
                 \parse [produce an AST directly from source], \
                 \check [*experimental* perform semantic analysis on an AST]" )
      <*> argument str
          (metavar "TARGET"
         <> help "A target .wat or .tok file" )
      <*> ( optional $ strOption
          $ long "out"
         <> short 'o'
         <> metavar "DIRECTORY"
         <> help "Optional output directory. Print to console if missing." )



-- MAIN


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Lexes and parses WebAssembly text or tokens"
     <> header "wat-frontend - a compiler frontend for WebAssembly text" )


run :: Args -> IO ()
run args =
    case args of
        (Args action target maybeOut) -> do
            case action of
                "lex"   -> do
                    case maybeOut of
                        Just outputDirectory -> do
                            writeLexOut target outputDirectory
                        Nothing ->
                            printLexOut target
                "parse" -> do
                    case maybeOut of
                        Just outputDirectory -> do
                            writeParseOut target outputDirectory
                        Nothing ->
                            printParseOut target
                "check" -> do
                    check target
                _   -> putStrLn flagsError


flagsError :: String
flagsError = "Command not available. Run with -h flag for help."
