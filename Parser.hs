import Lexer hiding (main, runTest)
import Prelude hiding (lex)
import System.Directory
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
  

-- MAIN


main :: IO ()
main = do
    tests <- listDirectory "tests/wat-types"
    mapM_ runTest tests

  
runTest :: String -> IO ()
runTest inputFile = do
    putStrLn inputFile
    text <- readFile $ "tests/wat-types/" ++ inputFile
    putStrLn text
    case Parsec.parse parse inputFile text of
        Left err  -> print err
        Right m   -> print m
    putStr "\n"



-- PARSE


parse :: Parsec.Parsec String () Module
parse = do
    Parsec.optional whitespace
    m <- betweenParens wasmModule
    Parsec.optional whitespace
    Parsec.eof
    return m 



-- MODULE


data Module = Module Components

instance Show Module where
    show (Module components) = "module\n" ++ showComponents 1 components


wasmModule :: Parsec.Parsec String () Module
wasmModule = do
    kw <- Parsec.lookAhead closeParen <|> keyword
    case kw of
        Keyword "module" -> do
            Parsec.optional whitespace
            cs <- components
            return (Module cs)
        CloseParen       -> return (Module (Components []))
        _                -> failStartsWithOrEmpty "module" "module" kw

    

-- COMPONENTS


data Components = Components { types :: [Component] }

data Component = Type (Maybe Ident) FuncType

components :: Parsec.Parsec String () Components
components = do
    ts <- Parsec.sepEndBy component whitespace  -- only types possible now, fix and sort for many component types
    return (Components ts)
    

component :: Parsec.Parsec String () Component
component = do
    c <- betweenParens $ Parsec.choice [ wasmType ] 
    return c



-- TYPES


data ValType = I32
             | I64
             | F32
             | F64

data FuncType = FuncType [Param] [Result]

data Param = Param (Maybe Ident) ValType

data Result = Result ValType

data Ident = Ident String

  
identify :: Maybe Token -> Maybe Ident
identify id =
    case id of
        Just (Id id) -> Just (Ident id)
        Nothing      -> Nothing


wasmType :: Parsec.Parsec String () Component
wasmType = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            Parsec.optional whitespace
            id <- Parsec.optionMaybe identifier
            Parsec.optional whitespace
            ft <- betweenParens funcType
            return (Type (identify id) ft)
        _              -> failStartsWith "type component" "type" kw


funcType :: Parsec.Parsec String () FuncType
funcType = do
    kw <- keyword
    case kw of
        Keyword "func" -> do
            Parsec.optional whitespace
            ps <- Parsec.sepEndBy (Parsec.try $ betweenParens param) whitespace
            Parsec.optional whitespace
            rs <- Parsec.sepEndBy (betweenParens result) whitespace
            return (FuncType ps rs)
        CloseParen       -> return (FuncType [] [])
        _                -> failStartsWith "function type" "func" kw
      

param :: Parsec.Parsec String () Param
param = do
    Parsec.lookAhead (Parsec.string "param")
    kw <- keyword
    case kw of
        Keyword "param" -> do
            whitespace
            id <- Parsec.optionMaybe identifier
            Parsec.optional whitespace
            vt <- valType
            return (Param (identify id) vt)
        _               -> failStartsWith "parameter" "param" kw
        

result :: Parsec.Parsec String () Result
result = do
    kw <- keyword
    case kw of
        Keyword "result" -> do
            whitespace
            vt <- valType
            return (Result vt)
        _               -> failStartsWith "result" "result" kw


valType :: Parsec.Parsec String () ValType
valType = do
    kw <- keyword
    case kw of
        Keyword "i32" -> return I32
        Keyword "i64" -> return I64
        Keyword "f32" -> return F32
        Keyword "f64" -> return F64
        _               -> Parsec.unexpected $
                                ": A value type must be \"i32\", \"i64\", \"f32\", or \"f64\" keyword\
                                \ but I am seeing \"" ++ (show kw) ++ "\""

  
betweenParens :: Parsec.Parsec String () a -> Parsec.Parsec String () a
betweenParens =
    Parsec.between (Parsec.char '(') (Parsec.char ')')



-- ERRORS


failStartsWith :: String -> String -> Token -> Parsec.Parsec String () a
failStartsWith production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \, but I am seeing \"" ++ (show actual) ++ "\""


failStartsWithOrEmpty :: String -> String -> Token -> Parsec.Parsec String () a
failStartsWithOrEmpty production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \ or be empty, but I am seeing \"" ++ (show actual) ++ "\""



-- SHOW


showComponents :: Int -> Components -> String
showComponents n components =
    indent n ++ "components\n"
        ++ concat (map (\c -> showComponent (n + 1) c) (types components))

  
showComponent :: Int -> Component -> String
showComponent n component =
    indent n  
        ++ case component of
               Type ident functype -> "type "
                                    ++ showMaybeIdent ident
                                    ++ "\n"
                                    ++ showFuncType (n + 1) functype


showValType :: ValType -> String
showValType valtype =
    case valtype of
        I32 -> "i32"
        I64 -> "i64"
        F32 -> "f32"
        F64 -> "f64"


showFuncType :: Int -> FuncType -> String
showFuncType n functype =
    indent n ++ "functype\n"
        ++ case functype of
               FuncType params results -> concat (map (\c -> showParam (n + 1) c ++ "\n") params)
                                              ++ concat (map (\c -> showResult (n + 1) c ++ "\n") results)

showParam :: Int -> Param -> String
showParam n param =
    indent n ++ "param "
        ++ case param of
               Param id valtype -> showMaybeIdent id ++ showValType valtype


showResult :: Int -> Result -> String
showResult n result =
    indent n ++ "result "
        ++ case result of
               Result valtype -> showValType valtype


instance Show Ident where
    show (Ident id) = id


showMaybeIdent :: Maybe Ident -> String
showMaybeIdent ident =
    case ident of
        Just id -> show id ++ " "
        Nothing -> ""


indent :: Int -> String
indent n =
    concat (replicate n "  ")
