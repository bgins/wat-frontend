{-# LANGUAGE FlexibleInstances #-}

module Parser where

import System.Directory
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))

import Lexer hiding (runTest)



-- PARSE

type ParserIdX = Either Int Ident

parse :: Parser (Module ParserIdX)
parse = do
    m <- wrapSpace $ betweenParens wasmModule
    Parsec.eof
    return m


parserIdX :: Token -> ParserIdX
parserIdX token =
    case token of
        UIntLit n -> Left n  -- TODO: check this is u32
        Id id     -> Right (Ident id)  -- TODO: check for it in identifier context


betweenParens :: Parser a -> Parser a
betweenParens =
    Parsec.between (Parsec.char '(') (Parsec.char ')')


wrapSpace :: Parser a -> Parser a
wrapSpace p = do
    Parsec.optional whitespace
    x <- p
    Parsec.optional whitespace
    return x


leftPad :: Parser a -> Parser a
leftPad p =
    whitespace >> p


toInt :: Token -> Int
toInt token =
    case token of
        UIntLit num -> num
        SIntLit num -> num

toModuleName :: Token -> ModuleName
toModuleName token =
    case token of
        StringLit str -> ModuleName str


toName :: Token -> Name
toName token =
    case token of
        StringLit str -> Name str


identify :: Maybe Token -> Maybe Ident
identify id =
    case id of
        Just (Id id) -> Just (Ident id)
        Nothing      -> Nothing


-- MODULE


data Module id = Module MaybeIdent (Components id)

wasmModule :: Parser (Module ParserIdX)
wasmModule = do
    mod <- Parsec.lookAhead closeParen <|> keyword
    case mod of
        Keyword "module" -> do
            id <- leftPad $ Parsec.optionMaybe identifier
            cs <- wrapSpace $ components
            return (Module (identify id) cs)
        CloseParen       -> return (Module Nothing [])
        _                -> failStartsWithOrEmpty "module" "module" mod



-- COMPONENTS


type Components id = [Component id]

data Component id = Type MaybeIdent FuncType
                  | Import ModuleName Name (ImportDescription id)
                  | Func MaybeIdent (TypeUse id) Locals (Instructions id)
                  | Start id
                  | Global MaybeIdent GlobalType (Instructions id)
                  | Export Name (ExportDescription id)

components :: Parser (Components ParserIdX)
components =
    Parsec.sepEndBy (betweenParens component) whitespace


component :: Parser (Component ParserIdX)
component = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            id <- wrapSpace $ Parsec.optionMaybe identifier
            ft <- betweenParens funcType
            return (Type (identify id) ft)
        Keyword "import" -> do
            mod <- leftPad $ string
            nm <- leftPad $ string
            impdesc <- leftPad $ betweenParens importdesc
            return (Import (toModuleName mod) (toName nm) impdesc)
        Keyword "func" -> do
            id <- wrapSpace $ Parsec.optionMaybe identifier
            typeuse <- typeUse
            locals <- locals
            instructions <- instructions
            return (Func (identify id) typeuse locals instructions)
        Keyword "start" -> do
            funcidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (Start $ parserIdX funcidx)
        Keyword "global" -> do
            id <- leftPad $ Parsec.optionMaybe identifier
            globaltype <- wrapSpace $ globalType
            instructions <- leftPad $ instructions
            return (Global (identify id) globaltype instructions)
        Keyword "export" -> do
            nm <- leftPad $ string
            expdesc <- leftPad $ betweenParens exportdesc
            return (Export (toName nm) expdesc)
        _              -> failStartsWith "component" "type or func" kw




-- TYPES


data FuncType = FuncType Params Results

data GlobalType = GlobalConst ValType
                | GlobalVar ValType

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
            id <- leftPad $ Parsec.optionMaybe identifier
            vt <- leftPad $ valType
            return (Param (identify id) vt)
        _               -> failStartsWith "parameter" "param" kw


result :: Parser Result
result = do
    Parsec.lookAhead (Parsec.string "result")
    kw <- keyword
    case kw of
        Keyword "result" -> do
            vt <- leftPad $ valType
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


globalType :: Parser GlobalType
globalType = do
    Parsec.try (betweenParens globalVar) <|> globalConst


globalConst :: Parser GlobalType
globalConst = do
    vt <- valType
    return (GlobalConst vt)


globalVar :: Parser GlobalType
globalVar = do
    kw <- keyword
    case kw of
        Keyword "mut" -> do
            vt <- leftPad $ valType
            return (GlobalVar vt)
        _             -> Parsec.unexpected $
                             ": a global can be marked as mutable with \"mut\" or\
                             \ the type alone for a constant"



-- TYPE USE


data TypeUse id = TypeUse id
                | TypeUseWithDeclarations id Params Results
                | InlineType Params Results

typeUse :: Parser (TypeUse ParserIdX)
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


typeRef :: Parser ParserIdX
typeRef = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            typeidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (parserIdX typeidx)
        _              -> failStartsWith "type use" "type" kw



-- IMPORTS


data ModuleName = ModuleName String

data Name = Name String

data ImportDescription id = FuncImport MaybeIdent (TypeUse id)
                       -- table, memory, and global imports go here

importdesc :: Parser (ImportDescription ParserIdX)
importdesc = do
    kw <- keyword
    case kw of
        Keyword "func" -> do
            id <- leftPad $ Parsec.optionMaybe identifier
            typeuse <- leftPad $ typeUse
            return (FuncImport (identify id) typeuse)



-- FUNCS


type Locals = [Local]

data Local = Local MaybeIdent ValType

type Instructions id = [Instruction id]

data Signedness = Signed
                | Unsigned

data Instruction id = Block MaybeIdent ResultType (Instructions id) MaybeIdent
                    | Loop MaybeIdent ResultType (Instructions id) MaybeIdent
                    | Conditional MaybeIdent ResultType (Instructions id) MaybeIdent (Instructions id) MaybeIdent
                    | Unreachable
                    | Nop
                    | Br id
                    | BrIf id
                    | BrTable [id] id
                    | Return
                    | Call id
                    | CallIndirect (TypeUse id)

                    -- parametric instructions
                    | Drop
                    | Select

                    -- variable instructions
                    | LocalGet id
                    | LocalSet id
                    | LocalTee id
                    | GlobalGet id
                    | GlobalSet id

                    -- add memory instructions

                    -- numeric instructions
                    | I32Const Int
                    | I64Const Int
                    | F32Const Float
                    | F64Const Double

                    | I32Clz
                    | I32Ctz
                    | I32Popcnt
                    | I32Add
                    | I32Sub
                    | I32Mul
                    | I32Div Signedness
                    | I32Rem Signedness
                    | I32And
                    | I32Or
                    | I32Xor
                    | I32Shl
                    | I32Shr Signedness
                    | I32Rotl
                    | I32Rotr

                    | I64Clz
                    | I64Ctz
                    | I64Popcnt
                    | I64Add
                    | I64Sub
                    | I64Mul
                    | I64Div Signedness
                    | I64Rem Signedness
                    | I64And
                    | I64Or
                    | I64Xor
                    | I64Shl
                    | I64Shr Signedness
                    | I64Rotl
                    | I64Rotr

                    | F32Abs
                    | F32Neg
                    | F32Ceil
                    | F32Floor
                    | F32Trunc
                    | F32Nearest
                    | F32Sqrt
                    | F32Add
                    | F32Sub
                    | F32Mul
                    | F32Div
                    | F32Min
                    | F32Max
                    | F32Copysign

                    | F64Abs
                    | F64Neg
                    | F64Ceil
                    | F64Floor
                    | F64Trunc
                    | F64Nearest
                    | F64Sqrt
                    | F64Add
                    | F64Sub
                    | F64Mul
                    | F64Div
                    | F64Min
                    | F64Max
                    | F64Copysign

                    | I32Eqz
                    | I32Eq
                    | I32Ne
                    | I32Lt Signedness
                    | I32Gt Signedness
                    | I32Le Signedness
                    | I32Ge Signedness

                    | I64Eqz
                    | I64Eq
                    | I64Ne
                    | I64Lt Signedness
                    | I64Gt Signedness
                    | I64Le Signedness
                    | I64Ge Signedness

                    | F32Eq
                    | F32Ne
                    | F32Lt
                    | F32Gt
                    | F32Le
                    | F32Ge

                    | F64Eq
                    | F64Ne
                    | F64Lt
                    | F64Gt
                    | F64Le
                    | F64Ge

                    | I32WrapI64
                    | I32TruncF32 Signedness
                    | I32TruncF64 Signedness
                    | I64ExtendI32 Signedness
                    | I64TruncF32 Signedness
                    | I64TruncF64 Signedness
                    | F32ConvertI32 Signedness
                    | F32ConvertI64 Signedness
                    | F32DemoteF64
                    | F64ConvertI32 Signedness
                    | F64ConvertI64 Signedness
                    | F64PromoteF32
                    | I32ReinterpretF32
                    | I64ReinterpretF64
                    | F32ReinterpretI32
                    | F64ReinterpretI64


type ResultType = Maybe Result

locals :: Parser Locals
locals =
    concat <$> Parsec.sepEndBy (betweenParens local) whitespace


local :: Parser [Local]
local = do
    kw <- keyword
    case kw of
        Keyword "local" -> do
            id <- leftPad $ Parsec.optionMaybe identifier
            case id of
                Just _ -> do
                    vt <- leftPad $ valType
                    return ([Local (identify id) vt])
                Nothing -> do
                    vts <- Parsec.sepEndBy valType whitespace
                    return (map (Local Nothing) vts)
        _              -> failStartsWith "local" "local" kw


instructions :: Parser (Instructions ParserIdX)
instructions =
    Parsec.sepEndBy instruction whitespace


block :: Parser (Instruction ParserIdX)
block = do
    label <- leftPad $ Parsec.optionMaybe identifier
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    instructions <- Parsec.manyTill (leftPad $ instruction) (Parsec.try end)
    id <- leftPad $ Parsec.optionMaybe identifier
    if label == id || id == Nothing then
        return (Block (identify label) resulttype instructions (identify id))
    else
        Parsec.unexpected $ ": block label and trailing id must match"


loop :: Parser (Instruction ParserIdX)
loop = do
    label <- leftPad $ Parsec.optionMaybe identifier
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    instructions <- Parsec.manyTill (leftPad $ instruction) (Parsec.try end)
    id <- leftPad $ Parsec.optionMaybe identifier
    if label == id || id == Nothing then
        return (Loop (identify label) resulttype instructions (identify id))
    else
        Parsec.unexpected $ ": loop label and trailing id must match"


conditional :: Parser (Instruction ParserIdX)
conditional = do
    label <- leftPad $ Parsec.optionMaybe identifier
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    ifinstrs <- Parsec.manyTill (leftPad $ instruction) (Parsec.try endOrElse)
    firstId <- leftPad $ Parsec.optionMaybe identifier
    elseinstrs <- leftPad $ Parsec.option [] elseInstructions
    secondId <- leftPad $ Parsec.optionMaybe identifier
    if (label == firstId && firstId == secondId)
       || (label == firstId && secondId == Nothing)
       || (label == secondId && firstId == Nothing)
       || (firstId == Nothing && secondId == Nothing) then
        return (Conditional (identify label) resulttype ifinstrs (identify firstId) elseinstrs (identify secondId))
    else
        Parsec.unexpected $ ": if label and trailing ids must match"


elseInstructions :: Parser (Instructions ParserIdX)
elseInstructions = do
    Parsec.manyTill (leftPad $ instruction) (Parsec.try end)


endOrElse :: Parser String
endOrElse = do
    leftPad $ Parsec.try (Parsec.string "end") <|> Parsec.string "else"


end :: Parser String
end = do
    leftPad $ Parsec.string "end"


instruction :: Parser (Instruction ParserIdX)
instruction = do
    kw <- keyword
    case kw of
        -- control instructions
        Keyword "block"         -> block
        Keyword "loop"          -> loop
        Keyword "if"            -> conditional
        Keyword "unreachable"   -> return Unreachable
        Keyword "nop"           -> return Nop
        Keyword "br"            -> do
            labelidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (Br $ parserIdX labelidx)
        Keyword "br_if"         -> do
            labelidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (BrIf $ parserIdX labelidx)
        Keyword "br_table"      -> return Nop
        Keyword "return"        -> return Return
        Keyword "call"          -> do
            funcidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (Call $ parserIdX funcidx)
        Keyword "call_indirect" -> do
            typeuse <- leftPad $ typeUse
            return (CallIndirect typeuse)

        -- parametric intsructions
        Keyword "drop"          -> return Drop
        Keyword "select"        -> return Select

        -- variable instructions
        Keyword "local.get"     -> do
            localidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (LocalGet $ parserIdX localidx)
        Keyword "local.set"     -> do
            localidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (LocalSet $ parserIdX localidx)
        Keyword "local.tee"     -> do
            localidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (LocalTee $ parserIdX localidx)
        Keyword "global.get"     -> do
            localidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (GlobalGet $ parserIdX localidx)
        Keyword "global.set"     -> do
            localidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (GlobalSet $ parserIdX localidx)

        -- add memory instructions

        -- numeric instructions
        Keyword "i32.const"           -> do
            int <- leftPad $ signedInteger <|> unsignedInteger
            return (I32Const $ toInt int)
        Keyword "i64.const"           -> do
            int <- leftPad $ signedInteger <|> unsignedInteger
            return (I64Const $ toInt int)
        Keyword "f32.const"           ->
            Parsec.unexpected $ ": Floats not implemented at f32.const"
        Keyword "f64.const"           ->
            Parsec.unexpected $ ": Floats not implemented at f64.const"

        Keyword "i32.clz"             -> return I32Clz
        Keyword "i32.ctz"             -> return I32Ctz
        Keyword "i32.popcnt"          -> return I32Popcnt
        Keyword "i32.add"             -> return I32Add
        Keyword "i32.sub"             -> return I32Sub
        Keyword "i32.mul"             -> return I32Mul
        Keyword "i32.div_s"           -> return (I32Div Signed)
        Keyword "i32.div_u"           -> return (I32Div Unsigned)
        Keyword "i32.rem_s"           -> return (I32Rem Signed)
        Keyword "i32.rem_u"           -> return (I32Rem Unsigned)
        Keyword "i32.and"             -> return I32And
        Keyword "i32.or"              -> return I32Or
        Keyword "i32.xor"             -> return I32Xor
        Keyword "i32.shl"             -> return I32Shl
        Keyword "i32.shr_s"           -> return (I32Shr Signed)
        Keyword "i32.shr_u"           -> return (I32Shr Unsigned)
        Keyword "i32.rotl"            -> return I32Rotl
        Keyword "i32.rotr"            -> return I32Rotr

        Keyword "i64.clz"             -> return I64Clz
        Keyword "i64.ctz"             -> return I64Ctz
        Keyword "i64.popcnt"          -> return I64Popcnt
        Keyword "i64.add"             -> return I64Add
        Keyword "i64.sub"             -> return I64Sub
        Keyword "i64.mul"             -> return I64Mul
        Keyword "i64.div_s"           -> return (I64Div Signed)
        Keyword "i64.div_u"           -> return (I64Div Unsigned)
        Keyword "i64.rem_s"           -> return (I64Rem Signed)
        Keyword "i64.rem_u"           -> return (I64Rem Unsigned)
        Keyword "i64.and"             -> return I64And
        Keyword "i64.or"              -> return I64Or
        Keyword "i64.xor"             -> return I64Xor
        Keyword "i64.shl"             -> return I64Shl
        Keyword "i64.shr_s"           -> return (I64Shr Signed)
        Keyword "i64.shr_u"           -> return (I64Shr Unsigned)
        Keyword "i64.rotl"            -> return I64Rotl
        Keyword "i64.rotr"            -> return I64Rotr

        Keyword "f32.abs"             -> return F32Abs
        Keyword "f32.neg"             -> return F32Neg
        Keyword "f32.ceil"            -> return F32Ceil
        Keyword "f32.floor"           -> return F32Floor
        Keyword "f32.trunc"           -> return F32Trunc
        Keyword "f32.nearest"         -> return F32Nearest
        Keyword "f32.sqrt"            -> return F32Sqrt
        Keyword "f32.add"             -> return F32Add
        Keyword "f32.sub"             -> return F32Sub
        Keyword "f32.mul"             -> return F32Mul
        Keyword "f32.div"             -> return F32Div
        Keyword "f32.min"             -> return F32Min
        Keyword "f32.max"             -> return F32Max
        Keyword "f32.copysign"        -> return F32Copysign

        Keyword "f64.abs"             -> return F64Abs
        Keyword "f64.neg"             -> return F64Neg
        Keyword "f64.ceil"            -> return F64Ceil
        Keyword "f64.floor"           -> return F64Floor
        Keyword "f64.trunc"           -> return F64Trunc
        Keyword "f64.nearest"         -> return F64Nearest
        Keyword "f64.sqrt"            -> return F64Sqrt
        Keyword "f64.add"             -> return F64Add
        Keyword "f64.sub"             -> return F64Sub
        Keyword "f64.mul"             -> return F64Mul
        Keyword "f64.div"             -> return F64Div
        Keyword "f64.min"             -> return F64Min
        Keyword "f64.max"             -> return F64Max
        Keyword "f64.copysign"        -> return F64Copysign

        Keyword "i32.eqz"             -> return I32Eqz
        Keyword "i32.eq"              -> return I32Eq
        Keyword "i32.ne"              -> return I32Ne
        Keyword "i32.lt_s"            -> return (I32Lt Signed)
        Keyword "i32.lt_u"            -> return (I32Lt Unsigned)
        Keyword "i32.gt_s"            -> return (I32Gt Signed)
        Keyword "i32.gt_u"            -> return (I32Gt Unsigned)
        Keyword "i32.le_s"            -> return (I32Le Signed)
        Keyword "i32.le_u"            -> return (I32Le Unsigned)
        Keyword "i32.ge_s"            -> return (I32Ge Signed)
        Keyword "i32.ge_u"            -> return (I32Ge Unsigned)

        Keyword "i64.eqz"             -> return I64Eqz
        Keyword "i64.eq"              -> return I64Eq
        Keyword "i64.ne"              -> return I64Ne
        Keyword "i64.lt_s"            -> return (I64Lt Signed)
        Keyword "i64.lt_u"            -> return (I64Lt Unsigned)
        Keyword "i64.gt_s"            -> return (I64Gt Signed)
        Keyword "i64.gt_u"            -> return (I64Gt Unsigned)
        Keyword "i64.le_s"            -> return (I64Le Signed)
        Keyword "i64.le_u"            -> return (I64Le Unsigned)
        Keyword "i64.ge_s"            -> return (I64Ge Signed)
        Keyword "i64.ge_u"            -> return (I64Ge Unsigned)

        Keyword "f32.eq"              -> return F32Eq
        Keyword "f32.ne"              -> return F32Ne
        Keyword "f32.lt"              -> return F32Lt
        Keyword "f32.gt"              -> return F32Gt
        Keyword "f32.le"              -> return F32Le
        Keyword "f32.ge"              -> return F32Ge

        Keyword "f64.eq"              -> return F64Eq
        Keyword "f64.ne"              -> return F64Ne
        Keyword "f64.lt"              -> return F64Lt
        Keyword "f64.gt"              -> return F64Gt
        Keyword "f64.le"              -> return F64Le
        Keyword "f64.ge"              -> return F64Ge

        Keyword "i32.wrap_i64"        -> return I32WrapI64
        Keyword "i32.trunc_f32_s"     -> return (I32TruncF32 Signed)
        Keyword "i32.trunc_f32_u"     -> return (I32TruncF32 Unsigned)
        Keyword "i32.trunc_f64_s"     -> return (I32TruncF64 Signed)
        Keyword "i32.trunc_f64_u"     -> return (I32TruncF64 Unsigned)
        Keyword "i64.extend_i32_s"    -> return (I64ExtendI32 Signed)
        Keyword "i64.extend_i32_u"    -> return (I64ExtendI32 Unsigned)
        Keyword "i64.trunc_f32_s"     -> return (I64TruncF32 Signed)
        Keyword "i64.trunc_f32_u"     -> return (I64TruncF32 Unsigned)
        Keyword "i64.trunc_f64_s"     -> return (I64TruncF64 Signed)
        Keyword "i64.trunc_f64_u"     -> return (I64TruncF64 Unsigned)
        Keyword "f32.convert_i32_s"   -> return (F32ConvertI32 Signed)
        Keyword "f32.convert_i32_u"   -> return (F32ConvertI32 Unsigned)
        Keyword "f32.convert_i64_s"   -> return (F32ConvertI64 Signed)
        Keyword "f32.convert_i64_u"   -> return (F32ConvertI64 Unsigned)
        Keyword "f32.demote_f64"      -> return F32DemoteF64
        Keyword "f64.convert_i32_s"   -> return (F64ConvertI32 Signed)
        Keyword "f64.convert_i32_u"   -> return (F64ConvertI32 Unsigned)
        Keyword "f64.convert_i64_s"   -> return (F64ConvertI64 Signed)
        Keyword "f64.convert_i64_u"   -> return (F64ConvertI64 Unsigned)
        Keyword "f64.promote_f32"     -> return F64PromoteF32
        Keyword "i32.reinterpret_f32" -> return (I32ReinterpretF32)
        Keyword "i64.reinterpret_f64" -> return (I64ReinterpretF64)
        Keyword "f32.reinterpret_i32" -> return (F32ReinterpretI32)
        Keyword "f64.reinterpret_i64" -> return (F64ReinterpretI64)
        _             -> Parsec.unexpected $ ": unexpected instruction " ++ show kw



-- IMPORTS


data ExportDescription id = FuncExport id
                       -- table, memory, and global exports go here

exportdesc :: Parser (ExportDescription ParserIdX)
exportdesc = do
    kw <- keyword
    case kw of
        Keyword "func" -> do
            funcidx <- leftPad $ Parsec.try unsignedInteger <|> identifier
            return (FuncExport $ parserIdX funcidx)


-- TREE REPRESENTATION


data Tree = Node String [Tree]

leaf    :: String -> Tree
leaf str = Node str []


idxToTree :: ParserIdX -> Tree
idxToTree idx =
    case idx of
        Left n   -> toTree n
        Right id -> toTree id


maybeIdToTree :: MaybeIdent -> [Tree]
maybeIdToTree maybeId =
    case maybeId of
        Just id -> [toTree id]
        Nothing -> []

maybeResultTypeToTree :: ResultType -> [Tree]
maybeResultTypeToTree maybeResult =
    case maybeResult of
        Just result -> [toTree result]
        Nothing -> []

class ToTree a where toTree :: a -> Tree

instance ToTree (Module ParserIdX) where
    toTree (Module maybeId components) =
        Node "module" $ maybeIdToTree maybeId ++ map toTree components

instance ToTree (Component ParserIdX) where
    toTree (Type maybeId functype) =
        Node "type" $ maybeIdToTree maybeId ++ [toTree functype]
    toTree (Import mod name importdesc) =
        Node "import" [toTree mod, toTree name, toTree importdesc]
    toTree (Func maybeId typeuse locals instructions) =
        Node "func" $ maybeIdToTree maybeId ++  [toTree typeuse] ++ map toTree locals ++ map toTree instructions
    toTree (Global maybeId globaltype instructions) =
        Node "global" $ maybeIdToTree maybeId ++ [toTree globaltype] ++ map toTree instructions
    toTree (Start funcidx) =
        Node "start" [idxToTree funcidx]
    toTree (Export name exportdesc) =
        Node "export" [toTree name, toTree exportdesc]

instance ToTree FuncType where
    toTree (FuncType params results) =
        Node "functype" $ map toTree params ++ map toTree results

instance ToTree Param where
    toTree (Param maybeId valtype) =
        Node "param" $ maybeIdToTree maybeId ++ [toTree valtype]

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

instance ToTree GlobalType where
    toTree (GlobalVar valtype) =
        Node "mut" [toTree valtype]
    toTree (GlobalConst valtype) =
        Node "const" [toTree valtype]

instance ToTree Int where
    toTree n = leaf (show n)

instance ToTree Float where
    toTree n = leaf (show n)

instance ToTree Double where
    toTree n = leaf (show n)

instance ToTree String where
    toTree str = leaf str

instance ToTree ModuleName where
    toTree (ModuleName name) = leaf (show name)

instance ToTree Name where
    toTree (Name name) = leaf (show name)

instance ToTree (ImportDescription ParserIdX) where
    toTree (FuncImport maybeId typeuse) =
        Node "func" $ maybeIdToTree maybeId ++ [toTree typeuse]

instance ToTree (TypeUse ParserIdX) where
    toTree (TypeUse typeidx) =
        Node "typuse" [idxToTree typeidx]
    toTree (TypeUseWithDeclarations typeidx params results) =
        Node "typuse" $ (idxToTree typeidx) : map toTree params ++ map toTree results
    toTree (InlineType params results) =
        Node "typuse" $ map toTree params ++ map toTree results

instance ToTree Local where
    toTree (Local maybeId valtype) =
        Node "local" $ maybeIdToTree maybeId ++ [toTree valtype]

instance ToTree (Instruction ParserIdX) where
    -- control instructions
    toTree (Block label resulttype instructions id) =
        Node "block" $
            maybeIdToTree label ++ maybeResultTypeToTree resulttype ++ map toTree instructions ++ maybeIdToTree id 
    toTree (Loop label resulttype instructions id) =
        Node "loop" $
            maybeIdToTree label ++ maybeResultTypeToTree resulttype ++ map toTree instructions ++ maybeIdToTree id 
    toTree (Conditional label resulttype ifInstructions ifId elseInstructions elseId) =
        Node "if" $
            maybeIdToTree label ++ maybeResultTypeToTree resulttype ++ map toTree ifInstructions ++ maybeIdToTree ifId 
                ++ case elseInstructions of
                      i:is -> [toTree "else"] ++ map toTree elseInstructions ++ maybeIdToTree elseId
                      []   -> []
    toTree Unreachable = leaf "unreachable"
    toTree Nop = leaf "nop"
    toTree (Br labelidx) =
        Node "br" [idxToTree labelidx]
    toTree (BrIf labelidx) =
        Node "br_if" [idxToTree labelidx]
    toTree (BrTable labelidxs labelidx) =
        Node "br_table" []
    toTree Return = leaf "return"
    toTree (Call funcidx) =
        Node "call" [idxToTree funcidx]
    toTree (CallIndirect typeuse) =
        Node "call_indirect" [toTree typeuse]
    toTree Drop = leaf "drop"
    toTree Select = leaf "select"

    -- variable instructions
    toTree (LocalGet localidx) =
        Node "local.get" [idxToTree localidx]
    toTree (LocalSet localidx) =
        Node "local.set" [idxToTree localidx]
    toTree (LocalTee localidx) =
        Node "local.tee" [idxToTree localidx]
    toTree (GlobalGet globalidx) =
        Node "global.get" [idxToTree globalidx]
    toTree (GlobalSet globalidx) =
        Node "global.set" [idxToTree globalidx]

    -- add memory instructions

    -- numeric instructions
    toTree (I32Const n) =
        Node "i32.const" [toTree n]
    toTree (I64Const n) =
        Node "i64.const" [toTree n]
    toTree (F32Const n) =
        Node "f32.const" [toTree n]
    toTree (F64Const n) =
        Node "f64.const" [toTree n]

    toTree I32Clz        = leaf "i32.clz"
    toTree I32Ctz        = leaf "i32.ctz"
    toTree I32Popcnt     = leaf "i32.popcnt"
    toTree I32Add        = leaf "i32.add"
    toTree I32Sub        = leaf "i32.sub"
    toTree I32Mul        = leaf "i32.mul"
    toTree (I32Div sgnd) = leaf $ "i32.div" ++ show sgnd
    toTree (I32Rem sgnd) = leaf $ "i32.rem" ++ show sgnd
    toTree I32And        = leaf "i32.and"
    toTree I32Or         = leaf "i32.or"
    toTree I32Xor        = leaf "i32.xor"
    toTree I32Shl        = leaf "i32.shl"
    toTree (I32Shr sgnd) = leaf $ "i32.shr" ++ show sgnd
    toTree I32Rotl       = leaf "i32.rotl"
    toTree I32Rotr       = leaf "i32.rotr"

    toTree I64Clz        = leaf "i64.clz"
    toTree I64Ctz        = leaf "i64.ctz"
    toTree I64Popcnt     = leaf "i64.popcnt"
    toTree I64Add        = leaf "i64.add"
    toTree I64Sub        = leaf "i64.sub"
    toTree I64Mul        = leaf "i64.mul"
    toTree (I64Div sgnd) = leaf $ "i64.div" ++ show sgnd
    toTree (I64Rem sgnd) = leaf $ "i64.rem" ++ show sgnd
    toTree I64And        = leaf "i64.and"
    toTree I64Or         = leaf "i64.or"
    toTree I64Xor        = leaf "i64.xor"
    toTree I64Shl        = leaf "i64.shl"
    toTree (I64Shr sgnd) = leaf $ "i64.shr" ++ show sgnd
    toTree I64Rotl       = leaf "i64.rotl"
    toTree I64Rotr       = leaf "i64.rotr"

    toTree F32Abs      = leaf "f32.abs"
    toTree F32Neg      = leaf "f32.neg"
    toTree F32Ceil     = leaf "f32.ceil"
    toTree F32Floor    = leaf "f32.floor"
    toTree F32Trunc    = leaf "f32.trunc"
    toTree F32Nearest  = leaf "f32.nearest"
    toTree F32Sqrt     = leaf "f32.sqrt"
    toTree F32Add      = leaf "f32.add"
    toTree F32Sub      = leaf "f32.sub"
    toTree F32Mul      = leaf "f32.mul"
    toTree F32Div      = leaf "f32.div"
    toTree F32Min      = leaf "f32.min"
    toTree F32Max      = leaf "f32.max"
    toTree F32Copysign = leaf "f32.copysign"

    toTree F64Abs      = leaf "f64.abs"
    toTree F64Neg      = leaf "f64.neg"
    toTree F64Ceil     = leaf "f64.ceil"
    toTree F64Floor    = leaf "f64.floor"
    toTree F64Trunc    = leaf "f64.trunc"
    toTree F64Nearest  = leaf "f64.nearest"
    toTree F64Sqrt     = leaf "f64.sqrt"
    toTree F64Add      = leaf "f64.add"
    toTree F64Sub      = leaf "f64.sub"
    toTree F64Mul      = leaf "f64.mul"
    toTree F64Div      = leaf "f64.div"
    toTree F64Min      = leaf "f64.min"
    toTree F64Max      = leaf "f64.max"
    toTree F64Copysign = leaf "f64.copysign"

    toTree I32Eqz       = leaf "i32.eqz"
    toTree I32Eq        = leaf "i32.eq"
    toTree I32Ne        = leaf "i32.ne"
    toTree (I32Lt sgnd) = leaf $ "i32.lt" ++ show sgnd
    toTree (I32Gt sgnd) = leaf $ "i32.gt" ++ show sgnd
    toTree (I32Le sgnd) = leaf $ "i32.le" ++ show sgnd
    toTree (I32Ge sgnd) = leaf $ "i32.ge" ++ show sgnd

    toTree I64Eqz       = leaf "i64.eqz"
    toTree I64Eq        = leaf "i64.eq"
    toTree I64Ne        = leaf "i64.ne"
    toTree (I64Lt sgnd) = leaf $ "i64.lt" ++ show sgnd
    toTree (I64Gt sgnd) = leaf $ "i64.gt" ++ show sgnd
    toTree (I64Le sgnd) = leaf $ "i64.le" ++ show sgnd
    toTree (I64Ge sgnd) = leaf $ "i64.ge" ++ show sgnd

    toTree F32Eq = leaf "f32.eq"
    toTree F32Ne = leaf "f32.ne"
    toTree F32Lt = leaf "f32.lt"
    toTree F32Gt = leaf "f32.gt"
    toTree F32Le = leaf "f32.le"
    toTree F32Ge = leaf "f32.ge"

    toTree F64Eq = leaf "f64.eq"
    toTree F64Ne = leaf "f64.ne"
    toTree F64Lt = leaf "f64.lt"
    toTree F64Gt = leaf "f64.gt"
    toTree F64Le = leaf "f64.le"
    toTree F64Ge = leaf "f64.ge"

    toTree I32WrapI64           = leaf "i32.wrap_i64"
    toTree (I32TruncF32 sgnd)   = leaf $ "i32.trunc_f32" ++ show sgnd
    toTree (I32TruncF64 sgnd)   = leaf $ "i32.trunc_f64" ++ show sgnd
    toTree (I64ExtendI32 sgnd)  = leaf $ "i64.extend_i32" ++ show sgnd
    toTree (I64TruncF32 sgnd)   = leaf $ "i64.trunc_f32" ++ show sgnd
    toTree (I64TruncF64 sgnd)   = leaf $ "i64.trunc_f64" ++ show sgnd
    toTree (F32ConvertI32 sgnd) = leaf $ "f32.convert_i32" ++ show sgnd
    toTree (F32ConvertI64 sgnd) = leaf $ "f32.convert_i64" ++ show sgnd
    toTree F32DemoteF64         = leaf "f32.demote_f64"
    toTree (F64ConvertI32 sgnd) = leaf $ "f64.convert_i32" ++ show sgnd
    toTree (F64ConvertI64 sgnd) = leaf $ "f64.convert_i64" ++ show sgnd
    toTree F64PromoteF32        = leaf "f64.promote_f32"
    toTree I32ReinterpretF32    = leaf "i32.reinterpret_f32"
    toTree I64ReinterpretF64    = leaf "i64.reinterpret_f64"
    toTree F32ReinterpretI32    = leaf "f32.reinterpret_i32"
    toTree F64ReinterpretI64    = leaf "f64.reinterpret_i64"

instance ToTree (ExportDescription ParserIdX) where
    toTree (FuncExport funcidx) =
        Node "func" [idxToTree funcidx]



-- SHOW

instance Show Ident where
    show (Ident id) = id

instance Show ValType where
    show I32 = "i32"
    show I64 = "i64"
    show F32 = "f32"
    show F64 = "f64"

instance Show Signedness where
    show Signed   = "_s"
    show Unsigned = "_u"


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
