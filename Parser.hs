module Parser where

import System.Directory
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
  
import Lexer hiding (runTest)



-- PARSE


parse :: Parser Module
parse = do
    m <- wrapSpace $ betweenParens wasmModule
    Parsec.eof
    return m 


betweenParens :: Parser a -> Parser a
betweenParens =
    Parsec.between (Parsec.char '(') (Parsec.char ')')

wrapSpace :: Parser a -> Parser a
wrapSpace p = do Parsec.optional whitespace
                 x <- p
                 Parsec.optional whitespace
                 return x

-- MODULE


data Module = Module Components

-- data Module a = Module Components a
-- type ParserId = Either Ident Int

wasmModule :: Parser Module
wasmModule = do
    kw <- Parsec.lookAhead closeParen <|> keyword
    case kw of
        Keyword "module" -> do
            Parsec.optional whitespace
            cs <- components
            return (Module cs)
        CloseParen       -> return (Module [])
        _                -> failStartsWithOrEmpty "module" "module" kw

    

-- COMPONENTS


type Components = [Component]

data Component = Type MaybeIdent FuncType
               -- | Func MaybeIdent TypeUse Locals Instructions
               | Func MaybeIdent TypeUse

components :: Parser Components
components = do
    cs <- Parsec.sepEndBy (betweenParens component) whitespace 
    return cs
    

component :: Parser Component
component = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            id <- wrapSpace $ Parsec.optionMaybe identifier
            ft <- betweenParens funcType
            return (Type (identify id) ft)
        Keyword "func" -> do
            id <- wrapSpace $ Parsec.optionMaybe identifier
            typeuse <- typeUse
            return (Func (identify id) typeuse)
        _              -> failStartsWith "component" "type or func" kw




-- TYPES


data FuncType = FuncType Params Results

type Params = [Param]

data Param = Param MaybeIdent ValType deriving (Eq)

type Results = [Result]

data Result = Result ValType deriving (Eq)

data Ident = Ident String deriving (Eq)

type MaybeIdent = Maybe Ident

data ValType = I32
             | I64
             | F32
             | F64 deriving (Eq)

  
identify :: Maybe Token -> Maybe Ident
identify id =
    case id of
        Just (Id id) -> Just (Ident id)
        Nothing      -> Nothing


funcType :: Parser FuncType
funcType = do
    kw <- keyword
    case kw of
        Keyword "func" -> do
            ps <- wrapSpace $ Parsec.sepEndBy (Parsec.try $ betweenParens param) whitespace
            rs <- Parsec.sepEndBy (betweenParens result) whitespace
            return (FuncType ps rs)
        CloseParen       -> return (FuncType [] [])
        _                -> failStartsWith "function type" "func" kw
      

param :: Parser Param
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
        

result :: Parser Result
result = do
    kw <- keyword
    case kw of
        Keyword "result" -> do
            whitespace
            vt <- valType
            return (Result vt)
        _               -> failStartsWith "result" "result" kw


valType :: Parser ValType
valType = do
    kw <- keyword
    case kw of
        Keyword "i32" -> return I32
        Keyword "i64" -> return I64
        Keyword "f32" -> return F32
        Keyword "f64" -> return F64
        _             -> Parsec.unexpected $
                             ": A value type must be \"i32\", \"i64\", \"f32\", or \"f64\" keyword\
                             \ but I am seeing \"" ++ (show kw) ++ "\""

  

-- TYPE USE


type TypeIdX = Either Int Ident

data TypeUse = TypeUse TypeIdX
             | TypeUseWithDeclarations TypeIdX Params Results
             | InlineType Params Results

typeUse :: Parser TypeUse
typeUse = do
    typeidx <- Parsec.optionMaybe $ Parsec.try (betweenParens typeRef)
    ps      <- wrapSpace $ Parsec.sepEndBy (Parsec.try $ betweenParens param) whitespace
    rs      <- Parsec.sepEndBy (betweenParens result) whitespace
    case typeidx of
        Just typeidx -> if ps == [] && rs == [] then
                            return (TypeUse typeidx)
                        else
                            return (TypeUseWithDeclarations typeidx ps rs)
        Nothing      -> if ps /= [] || rs /= [] then
                            return (InlineType ps rs)
                        else
                            Parsec.unexpected $ "A func component must have have a type reference\
                                                   \ or an inline type signature"


typeRef :: Parser TypeIdX
typeRef = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            whitespace
            typeidx <- Parsec.try unsignedInteger <|> identifier
            case typeidx of
                UIntLit n -> return (Left n)  -- TODO: check this is u32
                Id id     -> return (Right (Ident id))  -- TODO: check for it in identifier context
        _              -> failStartsWith "type use" "type" kw



-- TREE REPRESENTATION


data Tree = Node String [Tree]

leaf    :: String -> Tree
leaf str = Node str []

class ToTree a where toTree :: a -> Tree

instance ToTree Module where
    toTree (Module components) =
        Node "module" $ map toTree components

instance ToTree Component where
    toTree (Type id functype) =
        Node "type" $ case id of
                          Just id -> [toTree id, toTree functype]
                          Nothing -> [toTree functype]
    toTree (Func id typeuse) =
        Node "func" $ case id of
                          Just id -> [toTree id, toTree typeuse]
                          Nothing -> [toTree typeuse]

instance ToTree FuncType where
    toTree (FuncType params results) =
        Node "functype" $ map toTree params ++ map toTree results

instance ToTree Param where
    toTree (Param id valtype) =
        Node "param" $ case id of
            Just id -> [toTree id, toTree valtype]
            Nothing -> [toTree valtype]

instance ToTree Result where
    toTree (Result valtype) =
        Node "result" [toTree valtype]

instance ToTree Ident where
    toTree (Ident id) = leaf id

instance ToTree ValType where
    toTree I32 = leaf "i32"
    toTree I64 = leaf "i64"
    toTree F32 = leaf "f32"
    toTree F64 = leaf "f64"

instance ToTree Int where
    toTree n = leaf (show n)

instance ToTree TypeUse where
    toTree (TypeUse typeidx) =
        Node "typuse" $ case typeidx of
                            Left n -> [toTree n]
                            Right id -> [toTree id]
    toTree (TypeUseWithDeclarations typeidx params results) =
        Node "typuse" $ case typeidx of
                            Left n -> [toTree n] ++ map toTree params ++ map toTree results
                            Right id -> [toTree id] ++ map toTree params ++ map toTree results
    toTree (InlineType params results) =
        Node "typuse" $ map toTree params ++ map toTree results



-- SHOW


indentTree :: FilePath -> Int -> Tree -> IO ()
indentTree path n (Node str children) = do
    appendFile path $ indent n ++ str ++ "\n"
    mapM_ (indentTree path (n+1)) children


indentOut :: ToTree a => FilePath -> a -> IO ()
indentOut path =
      (indentTree path 0) . toTree 


indent :: Int -> String
indent n =
    concat (replicate n "  ")



-- TEST


testParser :: String ->  IO ()
testParser testDirectory = do
    tests <- listDirectory testDirectory
    mapM_ (runTest testDirectory) tests


runTest :: String -> String -> IO ()
runTest testDirectory inputFile = do
    text <- readFile $ testDirectory ++ "/" ++ inputFile
    case Parsec.parse parse inputFile text of
        Left err  -> writeFile errPath $ show err
        Right out -> indentOut outPath out
  where testName = reverse $ drop 4 $ reverse inputFile
        errPath = testDirectory ++ "/" ++ testName ++ ".err"
        outPath = testDirectory ++ "/" ++ testName ++ ".out"



-- ERRORS


failStartsWith :: String -> String -> Token -> Parser a
failStartsWith production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \, but I am seeing \"" ++ (show actual) ++ "\""


failStartsWithOrEmpty :: String -> String -> Token -> Parser a
failStartsWithOrEmpty production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \ or be empty, but I am seeing \"" ++ (show actual) ++ "\""
