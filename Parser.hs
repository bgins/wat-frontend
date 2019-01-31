module Parser where

import Lexer hiding (runTest)
import System.Directory
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
  


-- TEST


testParser :: String ->  IO ()
testParser testDirectory = do
    tests <- listDirectory testDirectory
    mapM_ (runTest testDirectory) tests


runTest :: String -> String -> IO ()
runTest testDirectory inputFile = do
    putStrLn inputFile
    text <- readFile $ testDirectory ++ "/" ++ inputFile
    putStrLn text
    case Parsec.parse parse inputFile text of
        Left err  -> print err
        Right m   -> indentOut m
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

wasmModule :: Parsec.Parsec String () Module
wasmModule = do
    kw <- Parsec.lookAhead closeParen <|> keyword
    case kw of
        Keyword "module" -> do
            Parsec.optional whitespace
            cs <- components
            return (Module cs)
        CloseParen       -> return (Module (Components [] []))
        _                -> failStartsWithOrEmpty "module" "module" kw

    

-- COMPONENTS


data Components = Components { types :: [Component]
                             , funcs :: [Component]
                             }

data Component = Type (Maybe Ident) FuncType
               -- | Func (Maybe Ident) (Maybe TypeUse) [Param] [Result] [Local] [Instruction]
               | Func (Maybe Ident) (Maybe TypeUse) [Param] [Result]

components :: Parsec.Parsec String () Components
components = do
    cs <- Parsec.sepEndBy (betweenParens component) whitespace 
    -- sort these components out
    return (Components cs [])
    

component :: Parsec.Parsec String () Component
component = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            Parsec.optional whitespace
            id <- Parsec.optionMaybe identifier
            Parsec.optional whitespace
            ft <- betweenParens funcType
            return (Type (identify id) ft)
        Keyword "func" -> do
            Parsec.optional whitespace
            id <- Parsec.optionMaybe identifier
            Parsec.optional whitespace
            typeuse <- Parsec.optionMaybe $ Parsec.try (betweenParens typeUse)
            Parsec.optional whitespace
            ps <- Parsec.sepEndBy (Parsec.try $ betweenParens param) whitespace
            Parsec.optional whitespace
            rs <- Parsec.sepEndBy (betweenParens result) whitespace
            return (Func (identify id) typeuse ps rs)
        _              -> failStartsWith "component" "type or func" kw




-- TYPES


data FuncType = FuncType [Param] [Result]

data Param = Param (Maybe Ident) ValType

data Result = Result ValType

data Ident = Ident String

data ValType = I32
             | I64
             | F32
             | F64

  
identify :: Maybe Token -> Maybe Ident
identify id =
    case id of
        Just (Id id) -> Just (Ident id)
        Nothing      -> Nothing


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


-- TYPE USE


data TypeUse = TypeUse (Either Int Ident)

typeUse :: Parsec.Parsec String () TypeUse
typeUse = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            whitespace
            typeidx <- Parsec.try unsignedInteger <|> identifier
            case typeidx of
                UIntLit n -> return (TypeUse (Left n))  -- TODO: check this is u32
                Id id     -> return (TypeUse (Right (Ident id)))  -- TODO: check for it in identifier context
        _             -> failStartsWith "type use" "type" kw


-- TREE REPRESENTATION


data Tree = Node String [Tree]
          | Leaf String

class ToTree a where toTree :: a -> Tree

instance ToTree Module where
    toTree (Module components) = Node "module" [toTree components]

instance ToTree Components where
    toTree (Components types funcs) = Node "components" $ map toTree types

instance ToTree Component where
    toTree (Type id functype) =
        Node "type" $ case id of
                          Just id -> [toTree id, toTree functype]
                          Nothing -> [toTree functype]
    toTree (Func id typeuse params results) =
        Node "func" $ case id of
                          Just id -> [toTree id]
                                ++ case typeuse of
                                       Just typeuse -> [toTree typeuse]
                                                           ++ map toTree params
                                                           ++ map toTree results
                                       Nothing      -> map toTree params
                                                           ++ map toTree results
                          Nothing -> case typeuse of
                                         Just typeuse -> [toTree typeuse]
                                                             ++ map toTree params
                                                             ++ map toTree results
                                         Nothing      -> map toTree params
                                                             ++ map toTree results


instance ToTree FuncType where
    toTree (FuncType params results) = Node "functype" $ map toTree params ++ map toTree results

instance ToTree Param where
    toTree (Param id valtype) =
        Node "param" $ case id of
            Just id -> [toTree id, toTree valtype]
            Nothing -> [toTree valtype]

instance ToTree Result where
    toTree (Result valtype) = Node "result" [toTree valtype]

instance ToTree Ident where
    toTree (Ident id) = Leaf id

instance ToTree ValType where
    toTree I32 = Leaf "i32"
    toTree I64 = Leaf "i64"
    toTree F32 = Leaf "f32"
    toTree F64 = Leaf "f64"

instance ToTree Int where
    toTree n = Leaf (show n)

instance ToTree TypeUse where
    toTree (TypeUse typeidx) =
        Node "typuse" $ case typeidx of
                            Left n -> [toTree n]
                            Right id -> [toTree id]



-- SHOW

indentTree :: Int -> Tree -> IO ()
indentTree n tree =
    case tree of
        Node str children -> do
            putStrLn $ indent n ++ str
            mapM_ (indentTree (n+1)) children
        Leaf str          -> do
            putStrLn $ indent n ++ str
        -- Leaf str          -> do
            -- putStr $ " " ++ str ++ " "


indentOut :: ToTree a => a -> IO ()
indentOut =
      (indentTree 0) . toTree


indent :: Int -> String
indent n =
    concat (replicate n "  ")



-- ERRORS


failStartsWith :: String -> String -> Token -> Parsec.Parsec String () a
failStartsWith production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \, but I am seeing \"" ++ (show actual) ++ "\""


failStartsWithOrEmpty :: String -> String -> Token -> Parsec.Parsec String () a
failStartsWithOrEmpty production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \ or be empty, but I am seeing \"" ++ (show actual) ++ "\""


