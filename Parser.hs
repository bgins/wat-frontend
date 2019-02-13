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
               | Func MaybeIdent TypeUse Locals Instructions
               | Start FuncIdX

components :: Parser Components
components =
    Parsec.sepEndBy (betweenParens component) whitespace 


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
            locals <- locals
            instructions <- instructions
            return (Func (identify id) typeuse locals instructions)
        Keyword "start" -> do
            whitespace
            funcidx <- Parsec.try unsignedInteger <|> identifier
            -- can this be handled better?
            case funcidx of
                UIntLit n -> return (Start $ Left n)  -- TODO: must be u32
                Id id     -> return (Start $ Right (Ident id))  -- TODO: check for it in identifier context
        _              -> failStartsWith "component" "type or func" kw
            


-- INDICES

type FuncIdX = Either Int Ident

type LabelIdX = Either Int Ident

type LabelIdXs = [LabelIdX]

type TypeIdX = Either Int Ident


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
    Parsec.lookAhead (Parsec.string "result")
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


data TypeUse = TypeUse TypeIdX
             | TypeUseWithDeclarations TypeIdX Params Results
             | InlineType Params Results

typeUse :: Parser TypeUse
typeUse = do
    typeidx <- Parsec.optionMaybe $ Parsec.try (betweenParens typeRef)
    ps      <- wrapSpace $ Parsec.sepEndBy (Parsec.try $ betweenParens param) whitespace
    rs      <- Parsec.sepEndBy (Parsec.try $ betweenParens result) whitespace
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



-- FUNCS


type Locals = [Local]

data Local = Local MaybeIdent ValType

type Instructions = [Instruction]

-- data Instruction = PlainInstruction
--                  | BlockInstruction

-- data PlainInstruction = Unreachable
--                       | Nop
--                       | Br LabelIdX
--                       | BrIf LabelIdX
--                       | BrTable LabelIdXs LabelIdX
--                       | Return
--                       | Call FuncIdX
--                       | CallIndirect TypeUse
--                       | Drop
--                       | Select

-- data BlockInsruction = Block MaybeIdent ResultType Instructions MaybeIdent
--                      | Loop MaybeIdent ResultType Instructions MaybeIdent
--                      | Conditional MaybeIdent ResultType Instructions MaybeIdent Instructions MaybeIdent
                     

data Instruction = Block MaybeIdent ResultType Instructions MaybeIdent
                 | Loop MaybeIdent ResultType Instructions MaybeIdent
                 | Conditional MaybeIdent ResultType Instructions MaybeIdent Instructions MaybeIdent
                 | Unreachable
                 | Nop
                 | Br LabelIdX
                 | BrIf LabelIdX
                 | BrTable LabelIdXs LabelIdX
                 | Return
                 | Call FuncIdX
                 | CallIndirect TypeUse
                 | Drop
                 | Select

type ResultType = Maybe Result

locals :: Parser Locals
locals =
    concat <$> Parsec.sepEndBy (betweenParens local) whitespace 


local :: Parser [Local]
local = do
    kw <- keyword
    case kw of
        Keyword "local" -> do
            whitespace
            id <- Parsec.optionMaybe identifier
            case id of
                Just _ -> do
                    whitespace
                    vt <- valType
                    return ([Local (identify id) vt])
                Nothing -> do
                    vts <- Parsec.sepBy valType whitespace
                    Parsec.optional whitespace
                    return (map (Local Nothing) vts)
        _              -> failStartsWith "local" "local" kw
   

instructions :: Parser Instructions
instructions =
    Parsec.sepEndBy instruction whitespace  -- check for last local first?


instruction :: Parser Instruction
instruction = do
    kw <- keyword
    case kw of
        Keyword "block" -> return Nop
        Keyword "loop" -> return Nop
        Keyword "if" -> return Nop
        Keyword "unreachable" -> return Unreachable
        Keyword "nop" -> return Nop
        Keyword "br" -> return Nop
        Keyword "br_if" -> return Nop
        Keyword "br_table" -> return Nop
        Keyword "return" -> return Return
        Keyword "call" -> return Nop
        Keyword "call_indirect" -> return Nop
  



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
    toTree (Func id typeuse locals instructions) =
        Node "func" $ case id of
                          Just id -> [toTree id, toTree typeuse] ++ map toTree locals ++ map toTree instructions
                          Nothing -> [toTree typeuse] ++ map toTree locals ++ map toTree instructions
    toTree (Start funcidx) =
        Node "start" $ case funcidx of
                           Left n   -> [toTree n]
                           Right id -> [toTree id]

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
                            Left n   -> [toTree n]
                            Right id -> [toTree id]
    toTree (TypeUseWithDeclarations typeidx params results) =
        Node "typuse" $ case typeidx of
                            Left n   -> [toTree n] ++ map toTree params ++ map toTree results
                            Right id -> [toTree id] ++ map toTree params ++ map toTree results
    toTree (InlineType params results) =
        Node "typuse" $ map toTree params ++ map toTree results

instance ToTree Local where
    toTree (Local id valtype) =
        Node "local" $ case id of
            Just id -> [toTree id, toTree valtype]
            Nothing -> [toTree valtype]


instance ToTree Instruction where
    toTree (Block label resulttype instructions id) =
        Node "block" []
    toTree (Loop label resulttype instructions id) =
        Node "loop" []
    toTree (Conditional label resulttype ifInstructions ifId elseInstructions elseId) =
        Node "if" []
    toTree Unreachable =
        leaf "unreachable"
    toTree Nop =
        leaf "nop"
    toTree (Br labelIdX) =
        Node "br" []
    toTree (BrIf labelIdX) =
        Node "br_if" []
    toTree (BrTable labelIdXs labelIdX) =
        Node "br_table" []
    toTree Return =
        leaf "return"
    toTree (Call funcIdX) =
        Node "call" []
    toTree (CallIndirect typeUse) =
        Node "call_indirect" []
    toTree Drop =
        leaf "drop"
    toTree Select =
        leaf "select"

-- data Instruction = Block MaybeIdent ResultType Instructions MaybeIdent
--                  | Loop MaybeIdent ResultType Instructions MaybeIdent
--                  | Conditional MaybeIdent ResultType Instructions MaybeIdent Instructions MaybeIdent
--                  | Unreachable
--                  | Nop
--                  | Br LabelIdX
--                  | BrIf LabelIdX
--                  | BrTable LabelIdXs LabelIdX
--                  | Return
--                  | Call FuncIdX
--                  | CallIndirect TypeUse
--                  | Drop
--                  | Select


-- SHOW


indentTree :: FilePath -> Int -> Tree -> IO ()
indentTree path n (Node str children) = do
    appendFile path $ indent n ++ str ++ "\n"
    mapM_ (indentTree path (n+1)) children


indentOut :: ToTree a => FilePath -> a -> IO ()
indentOut path =
    indentTree path 0 . toTree 


indent :: Int -> String
indent n =
    replicate (2*n) ' '



-- TEST


testParser :: String ->  IO ()
testParser testDirectory = do
    tests <- listDirectory testDirectory
    mapM_ (runTest testDirectory) tests


runTest :: String -> String -> IO ()
runTest testDirectory inputFile = do
    text <- readFile $ testFile inputFile
    case Parsec.parse parse inputFile text of
        Left err  -> writeFile errPath $ show err
        Right out -> indentOut outPath out
  where testName   = reverse $ drop 4 $ reverse inputFile
        testFile s = testDirectory ++ "/" ++ s
        path       = testFile testName
        errPath    = path ++ ".err"
        outPath    = path ++ ".out"



-- ERRORS


failStartsWith :: String -> String -> Token -> Parser a
failStartsWith production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \, but I am seeing \"" ++ (show actual) ++ "\""


failStartsWithOrEmpty :: String -> String -> Token -> Parser a
failStartsWithOrEmpty production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \ or be empty, but I am seeing \"" ++ (show actual) ++ "\""
