{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Monad (guard)
import Data.Word (Word32, Word64)
import System.FilePath.Posix (takeFileName)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>),(<?>))

import Lexer

{-| This module contains a parser that generates ASTs from WebAssembly text source.

  Lexical analysis and parsing a performed in a single pass in parser combinator
  style. The parser depends on the parsers from Lexer.hs as primitives.

  This is an implementation of the WebAssembly spec released on January 10th,
  2019. Sections of the specification are referenced with § section marks
  throughout this module.

  Tables, Memories, Element Segments and Data Segments have not been
  implemented. In addition, Floating-point numbers have not been implemented.
-}



-- PARSE


parse :: Parser (Module ParserIdX)
parse = do
    m <- wrapSpace $ betweenParens parseModule
    Parsec.eof
    return m


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



-- INTEGERS [§6.3.1], IDENTIFIERS [§6.3.5], INDICES [§6.6.1], NAMES [§6.3.4]


type ParserIdX = Either Word32 Ident

data Ident = Ident String deriving (Eq)

type MaybeIdent = Maybe Ident


i32 :: Parser Word32
i32 = do
    int <- signedInteger <|> unsignedInteger
    case int of
        UIntLit n -> do
            guard (n >= 0 && n <= maxUInt32)
                <?> "int " ++ show n ++ " out of range. Expected a 32-bit integer."
            return (fromInteger n)
        SIntLit n -> do
            guard (n >= minSInt32 && n <= maxSInt32)
                <?> "int " ++ show n ++ " out of range. Expected a signed 32-bit integer."
            return (fromInteger n)
        _         -> Parsec.unexpected "Expected a UIntLit or an SIntLit token"


i64 :: Parser Word64
i64 = do
    int <- signedInteger <|> unsignedInteger
    case int of
        UIntLit n -> do
            guard (n >= 0 && n <= maxUInt64)
                <?> "int " ++ show n ++ " out of range. Expected a 64-bit integer."
            return (fromInteger n)
        SIntLit n -> do
            guard (n >= minSInt64 && n <= maxSInt64)
                <?> "int " ++ show n ++ " out of range. Expected a signed 64-bit integer."
            return (fromInteger n)
        _         -> Parsec.unexpected "Expected a UIntLit or an SIntLit token"


maybeIdent :: Parser (Maybe Ident)
maybeIdent = do
    id <- Parsec.optionMaybe identifier
    case id of
        Just (Id id) -> return (Just (Ident id))
        Nothing      -> return Nothing
        Just _       -> Parsec.unexpected "Expected and Id token"


parserIdX :: Parser ParserIdX
parserIdX = do
    idx <- Parsec.try unsignedInteger <|> identifier
    case idx of
        UIntLit n -> do
            guard (n >= 0 && n <= maxUInt32)
                <?> "index " ++ show n ++ " out of range. Ids must be 32-bit unsigned integers."
            return (Left $ fromInteger n)
        Id id     ->
            return (Right (Ident id))
        _         -> Parsec.unexpected "Expected a UIntLit or an Id token"


moduleName :: Parser ModuleName
moduleName = do
    name <- string
    case name of
        StringLit str ->
            return (ModuleName str)
        _         -> Parsec.unexpected "Expected a StringLit token"


name :: Parser Name
name = do
    name <- string
    case name of
        StringLit str ->
            return (Name str)
        _         -> Parsec.unexpected "Expected a StringLit token"


maxUInt32 :: Integer
maxUInt32 = 4294967295

minSInt32 :: Integer
minSInt32 = -2147483648

maxSInt32 :: Integer
maxSInt32 = 2147483647

maxUInt64 :: Integer
maxUInt64 = 18446744073709551615

minSInt64 :: Integer
minSInt64 = -9223372036854775808

maxSInt64 :: Integer
maxSInt64 = 9223372036854775807



-- VALUE TYPES [§6.4.1], PARAMS [§6.4.3], RESULTS [§6.4.3]


data ValType = I32
             | I64
             | F32
             | F64 deriving (Eq)

data Param = Param MaybeIdent ValType deriving (Eq)

type Params = [Param]

data Result = Result ValType deriving (Eq)

type Results = [Result]


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


param :: Parser Param
param = do
    Parsec.lookAhead (Parsec.string "param")
    kw <- keyword
    case kw of
        Keyword "param" -> do
            maybeId <- leftPad $ maybeIdent
            vt <- leftPad $ valType
            return (Param maybeId vt)
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



-- TYPES [§6.4]


data FuncType = FuncType Params Results deriving (Eq)

data GlobalType = GlobalConst ValType
                | GlobalVar ValType


parseType :: Parser (Component ParserIdX)
parseType = do
    maybeId <- wrapSpace $ maybeIdent
    ft <- betweenParens funcType
    return (Type maybeId ft)


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



-- TYPE USES [§6.6.3]

{-| InlineType implements the typeuse abbreviation that does not reference a
  pre-existing type
-}

data TypeUse idx = TypeUse idx
                | TypeUseWithDeclarations idx Params Results
                | InlineType Params Results

typeUse :: Parser (TypeUse ParserIdX)
typeUse = do
    typeidx <- Parsec.optionMaybe $ Parsec.try (betweenParens typeRef)
    ps      <- wrapSpace $ Parsec.sepEndBy (Parsec.try $ betweenParens param) whitespace
    rs      <- Parsec.sepEndBy (Parsec.try $ betweenParens result) whitespace
    case typeidx of
        Just typeidx -> if null ps && null rs then
                            return (TypeUse typeidx)
                        else
                            return (TypeUseWithDeclarations typeidx ps rs)
        Nothing      ->  return (InlineType ps rs)


typeRef :: Parser ParserIdX
typeRef = do
    kw <- keyword
    case kw of
        Keyword "type" -> do
            typeidx <- leftPad $ parserIdX
            return typeidx
        _              -> failStartsWith "type use" "type" kw



-- IMPORTS [§6.6.4]


data ModuleName = ModuleName String

data Name = Name String deriving (Eq)

data ImportDescription idx = FuncImport MaybeIdent (TypeUse idx)
                           | GlobalImport MaybeIdent GlobalType
                       -- add table, memory, and global imports here

parseImport :: Parser (Component ParserIdX)
parseImport = do
    mname <- leftPad $ moduleName
    name <- leftPad $ name
    impdesc <- leftPad $ betweenParens importdesc
    return (Import mname name impdesc)


importdesc :: Parser (ImportDescription ParserIdX)
importdesc = do
    kw <- keyword
    case kw of
        Keyword "func" -> do
            maybeId <- leftPad $ maybeIdent
            typeuse <- leftPad $ typeUse
            return (FuncImport maybeId typeuse)
        Keyword "global" -> do
            maybeId <- leftPad $ maybeIdent
            globaltype <- wrapSpace $ globalType
            return (GlobalImport maybeId globaltype)
        _         -> Parsec.unexpected "Expected a Keyword token"



-- FUNCS [§6.6.5]


type Locals = [Local]

data Local = Local MaybeIdent ValType

type Instructions idx = [Instruction idx]

data Signedness = Signed
                | Unsigned


                    -- control instructions [§6.5.2]
data Instruction idx = Block MaybeIdent ResultType (Instructions idx)
                    | Loop MaybeIdent ResultType (Instructions idx)
                    | Conditional MaybeIdent ResultType (Instructions idx) (Instructions idx)
                    | Unreachable
                    | Nop
                    | Br idx
                    | BrIf idx
                    | BrTable [idx] idx
                    | Return
                    | Call idx
                    | CallIndirect (TypeUse idx)

                    -- parametric instructions [§6.5.3]
                    | Drop
                    | Select

                    -- variable instructions [§6.5.4]
                    | LocalGet idx
                    | LocalSet idx
                    | LocalTee idx
                    | GlobalGet idx
                    | GlobalSet idx

                    -- add memory instructions [§6.5.5]

                    -- numeric instructions [§6.5.6]
                    | I32Const Word32
                    | I64Const Word64
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

parseFunc :: Parser (Component ParserIdX)
parseFunc = do
    maybeId <- wrapSpace $ maybeIdent
    typeuse <- typeUse
    locals <- locals
    instructions <- instructions
    return (Func maybeId typeuse locals instructions)


locals :: Parser Locals
locals =
    concat <$> Parsec.sepEndBy (betweenParens local) whitespace


local :: Parser [Local]
local = do
    kw <- keyword
    case kw of
        Keyword "local" -> do
            maybeId <- leftPad $ maybeIdent
            case maybeId of
                Just _ -> do
                    vt <- leftPad $ valType
                    return ([Local maybeId vt])
                Nothing -> do
                    vts <- Parsec.sepEndBy valType whitespace
                    return (map (Local Nothing) vts)
        _              -> failStartsWith "local" "local" kw


instructions :: Parser (Instructions ParserIdX)
instructions =
    Parsec.sepEndBy instruction whitespace


instruction :: Parser (Instruction ParserIdX)
instruction = do
    kw <- keyword
    case kw of
        -- control instructions [§6.5.2]
        Keyword "block"         -> block
        Keyword "loop"          -> loop
        Keyword "if"            -> Parsec.try ifElseEnd <|> ifEnd
        Keyword "unreachable"   -> return Unreachable
        Keyword "nop"           -> return Nop
        Keyword "br"            -> do
            labelidx <- leftPad $ parserIdX
            return (Br labelidx)
        Keyword "br_if"         -> do
            labelidx <- leftPad $ parserIdX
            return (BrIf labelidx)
        Keyword "br_table"      -> Parsec.unexpected ": \"br_table\" not implemented"
        Keyword "return"        -> return Return
        Keyword "call"          -> do
            funcidx <- leftPad $ parserIdX
            return (Call funcidx)
        Keyword "call_indirect" -> do
            typeuse <- leftPad $ typeUse
            return (CallIndirect typeuse)

        -- parametric instructions [§6.5.3]
        Keyword "drop"          -> return Drop
        Keyword "select"        -> return Select

        -- variable instructions [§6.5.4]
        Keyword "local.get"     -> do
            localidx <- leftPad $ parserIdX
            return (LocalGet localidx)
        Keyword "local.set"     -> do
            localidx <- leftPad $ parserIdX
            return (LocalSet localidx)
        Keyword "local.tee"     -> do
            localidx <- leftPad $ parserIdX
            return (LocalTee localidx)
        Keyword "global.get"     -> do
            localidx <- leftPad $ parserIdX
            return (GlobalGet localidx)
        Keyword "global.set"     -> do
            localidx <- leftPad $ parserIdX
            return (GlobalSet localidx)

        -- add memory instructions [§6.5.5]

        -- numeric instructions [§6.5.6]
        Keyword "i32.const"      -> do
            int <- leftPad $ i32
            return (I32Const int)
        Keyword "i64.const"      -> do
            int <- leftPad $ i64
            return (I64Const int)
        Keyword "f32.const"      ->
            Parsec.unexpected $ ": Floats not implemented at f32.const"
        Keyword "f64.const"      ->
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


block :: Parser (Instruction ParserIdX)
block = do
    maybeLabel <- leftPad $ maybeIdent
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    instructions <- Parsec.manyTill (leftPad $ instruction) (Parsec.try end)
    maybeId <- leftPad $ maybeIdent
    if maybeLabel == maybeId || maybeId == Nothing then
        return (Block maybeLabel resulttype instructions)
    else
        Parsec.unexpected $ ": block label and trailing id must match"


loop :: Parser (Instruction ParserIdX)
loop = do
    maybeLabel <- leftPad $ maybeIdent
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    instructions <- Parsec.manyTill (leftPad $ instruction) (Parsec.try end)
    maybeId <- leftPad $ maybeIdent
    if maybeLabel == maybeId || maybeId == Nothing then
        return (Loop maybeLabel resulttype instructions)
    else
        Parsec.unexpected $ ": loop label and trailing id must match"


ifElseEnd :: Parser (Instruction ParserIdX)
ifElseEnd = do
    maybeLabel <- leftPad $ maybeIdent
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    ifinstrs <- Parsec.manyTill (leftPad $ instruction) (Parsec.try $ (leftPad $ Parsec.string "else"))
    maybeElseId <- leftPad $ maybeIdent
    elseinstrs <- Parsec.manyTill (leftPad $ instruction) (Parsec.try $ end)
    maybeEndId <- leftPad $ maybeIdent
    if maybeElseId == Nothing
       || (maybeLabel == maybeElseId && maybeEndId == Nothing)
       || maybeLabel == maybeEndId then
        return (Conditional maybeLabel resulttype ifinstrs elseinstrs)
    else
        Parsec.unexpected $ ": if label and trailing ids must match"


ifEnd :: Parser (Instruction ParserIdX)
ifEnd = do
    maybeLabel <- leftPad $ maybeIdent
    resulttype <- leftPad $ Parsec.optionMaybe (betweenParens result)
    ifinstrs <- Parsec.manyTill (leftPad $ instruction) (Parsec.try end)
    maybeId <- leftPad $ maybeIdent
    if maybeLabel == maybeId || maybeId == Nothing then
        return (Conditional maybeLabel resulttype ifinstrs [])
    else
        Parsec.unexpected $ ": if label and trailing ids must match"


end :: Parser String
end = do
    leftPad $ Parsec.string "end"



-- START [§6.6.10]


parseStart :: Parser (Component ParserIdX)
parseStart = do
    funcidx <- leftPad $ parserIdX
    return (Start funcidx)



-- GLOBALS [§6.6.8]


parseGlobal :: Parser (Component ParserIdX)
parseGlobal = do
    maybeId <- leftPad $ maybeIdent
    globaltype <- wrapSpace $ globalType
    instructions <- leftPad $ instructions
    return (Global maybeId globaltype instructions)



-- EXPORTS [§6.6.9]


data ExportDescription idx = FuncExport idx
                           -- add table and memory here
                           | GlobalExport idx


parseExport :: Parser (Component ParserIdX)
parseExport = do
    name <- leftPad $ name
    expdesc <- leftPad $ betweenParens exportdesc
    return (Export name expdesc)


exportdesc :: Parser (ExportDescription ParserIdX)
exportdesc = do
    kw <- keyword
    case kw of
        Keyword "func" -> do
            funcidx <- leftPad $ parserIdX
            return (FuncExport funcidx)
        Keyword "global" -> do
            globalidx <- leftPad $ parserIdX
            return (GlobalExport globalidx)
        _         -> Parsec.unexpected "Expected a Keyword token"


-- MODULE [§6.6.13]


data Module idx = Module MaybeIdent (Components idx)

type Components idx = [Component idx]

data Component idx = Type MaybeIdent FuncType
                  | Import ModuleName Name (ImportDescription idx)
                  | Func MaybeIdent (TypeUse idx) Locals (Instructions idx)
                  | Start idx
                  | Global MaybeIdent GlobalType (Instructions idx)
                  | Export Name (ExportDescription idx)

parseModule :: Parser (Module ParserIdX)
parseModule = do
    mod <- Parsec.lookAhead closeParen <|> keyword
    case mod of
        Keyword "module" -> do
            maybeId <- leftPad $ maybeIdent
            cs <- wrapSpace $ components
            return (Module maybeId cs)
        CloseParen       -> return (Module Nothing [])
        _                -> failStartsWithOrEmpty "module" "module" mod


components :: Parser (Components ParserIdX)
components =
    Parsec.sepEndBy (betweenParens component) whitespace


component :: Parser (Component ParserIdX)
component = do
    kw <- keyword
    case kw of
        Keyword "type"   -> parseType
        Keyword "import" -> parseImport
        Keyword "func"   -> parseFunc
        Keyword "start"  -> parseStart
        Keyword "global" -> parseGlobal
        Keyword "export" -> parseExport
        _                -> failStartsWith "component" "type, import, func, start, global, or export" kw



-- TREE REPRESENTATION


data Tree = Node String [Tree]

leaf    :: String -> Tree
leaf str = Node str []


maybeResultTypeToTree :: ResultType -> [Tree]
maybeResultTypeToTree maybeResult =
    case maybeResult of
        Just result -> [toTree result]
        Nothing -> []


class ToTree a where toTree :: a -> Tree

instance ToTree Ident where
    toTree (Ident id) = leaf id

instance ToTree String where
    toTree str = leaf str

instance ToTree ValType where
    toTree I32 = leaf "i32"
    toTree I64 = leaf "i64"
    toTree F32 = leaf "f32"
    toTree F64 = leaf "f64"

instance ToTree Param where
    toTree (Param maybeId valtype) =
        leaf $ "param" ++ showMaybeId maybeId ++ show valtype

instance ToTree Result where
    toTree (Result valtype) =
        leaf $ "result" ++ show valtype

instance ToTree FuncType where
    toTree (FuncType params results) =
        Node "functype" $ map toTree params ++ map toTree results

instance ToTree GlobalType where
    toTree (GlobalVar valtype) =
        leaf $ "mut" ++ show valtype
    toTree (GlobalConst valtype) =
        leaf $ "const" ++ show valtype

instance ToTree (TypeUse ParserIdX) where
    toTree (TypeUse typeidx) =
        leaf $ "typeuse" ++ showIdX typeidx
    toTree (TypeUseWithDeclarations typeidx params results) =
        Node ("typeuse" ++ showIdX typeidx) $ map toTree params ++ map toTree results
    toTree (InlineType params results) =
        Node "typeuse" $ map toTree params ++ map toTree results

instance ToTree (ImportDescription ParserIdX) where
    toTree (FuncImport maybeId typeuse) =
        Node ("func" ++ showMaybeId maybeId) $ [toTree typeuse]
    toTree (GlobalImport maybeId globaltype) =
        Node ("global" ++ showMaybeId maybeId) $ [toTree globaltype]

instance ToTree Local where
    toTree (Local maybeId valtype) =
        leaf $ "local" ++ showMaybeId maybeId ++ show valtype

instance ToTree (Instruction ParserIdX) where
    -- control instructions
    toTree (Block label resulttype instructions) =
        Node ("block" ++ showMaybeId label)
            $ maybeResultTypeToTree resulttype ++ map toTree instructions
    toTree (Loop label resulttype instructions) =
        Node ("loop" ++ showMaybeId label)
            $ maybeResultTypeToTree resulttype ++ map toTree instructions
    toTree (Conditional label resulttype ifInstructions elseInstructions) =
        Node ("if" ++ showMaybeId label)
            $ maybeResultTypeToTree resulttype ++ map toTree ifInstructions
                ++ case elseInstructions of
                      i:is -> [toTree "else"] ++ map toTree elseInstructions
                      []   -> []
    toTree Unreachable = leaf "unreachable"
    toTree Nop = leaf "nop"
    toTree (Br labelidx) =
        leaf $ "br" ++ showIdX labelidx
    toTree (BrIf labelidx) =
        leaf $ "br_if" ++ showIdX labelidx
    toTree (BrTable labelidxs labelidx) =
        Node "br_table" []
    toTree Return = leaf "return"
    toTree (Call funcidx) =
        leaf $ "call" ++ showIdX funcidx
    toTree (CallIndirect typeuse) =
        Node "call_indirect" [toTree typeuse]
    toTree Drop = leaf "drop"
    toTree Select = leaf "select"

    -- variable instructions
    toTree (LocalGet localidx) =
        leaf $ "local.get" ++ showIdX localidx
    toTree (LocalSet localidx) =
        leaf $ "local.set" ++ showIdX localidx
    toTree (LocalTee localidx) =
        leaf $ "local.tee" ++ showIdX localidx
    toTree (GlobalGet globalidx) =
        leaf $ "global.get" ++ showIdX globalidx
    toTree (GlobalSet globalidx) =
        leaf $ "global.set" ++ showIdX globalidx

    -- add memory instructions

    -- numeric instructions
    toTree (I32Const n) =
        leaf $ "i32.const" ++ (' ' : show n)
    toTree (I64Const n) =
        leaf $ "i64.const" ++ (' ' : show n)
    toTree (F32Const n) =
        leaf $ "f32.const" ++ (' ' : show n)
    toTree (F64Const n) =
        leaf $ "f64.const" ++ (' ' : show n)

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
        leaf $ "func" ++ showIdX funcidx
    toTree (GlobalExport globalidx) =
        leaf $ "global" ++ showIdX globalidx

instance ToTree (Module ParserIdX) where
    toTree (Module maybeId components) =
        Node ("module" ++ showMaybeId maybeId) $ map toTree components

instance ToTree (Component ParserIdX) where
    toTree (Type maybeId functype) =
        Node ("type" ++ showMaybeId maybeId) $ [toTree functype]
    toTree (Import mod name importdesc) =
        Node ("import"  ++ show mod ++ show name) $ [toTree importdesc]
    toTree (Func maybeId typeuse locals instructions) =
        Node ("func" ++ showMaybeId maybeId) $ [toTree typeuse] ++ map toTree locals ++ map toTree instructions
    toTree (Global maybeId globaltype instructions) =
        Node ("global" ++ showMaybeId maybeId) $ [toTree globaltype] ++ map toTree instructions
    toTree (Start funcidx) =
        leaf $ "start"  ++ showIdX funcidx
    toTree (Export name exportdesc) =
        Node ("export" ++ show name) $ [toTree exportdesc]



-- SHOW


instance Show Ident where
    show (Ident id) = id

instance Show ValType where
    show I32 = ' ' : "i32"
    show I64 = ' ' : "i64"
    show F32 = ' ' : "f32"
    show F64 = ' ' : "f64"

instance Show ModuleName where
    show (ModuleName name) = ' ' : show name

instance Show Name where
    show (Name name) = ' ' : show name

instance Show Signedness where
    show Signed   = "_s"
    show Unsigned = "_u"


showIdX :: ParserIdX -> String
showIdX idx =
    case idx of
        Left n   -> ' ' : show n
        Right id -> ' ' : show id


maybeIdToTree :: MaybeIdent -> [Tree]
maybeIdToTree maybeId =
    case maybeId of
        Just id -> [toTree id]
        Nothing -> []


showMaybeId :: MaybeIdent -> String
showMaybeId maybeId =
    case maybeId of
        Just id -> ' ' : show id
        Nothing -> ""



-- IO


indentTree :: Int -> Tree -> [String]
indentTree n (Node str children) =
    (indent n ++ str) : concat (map (indentTree (n+1)) children)


indent :: Int -> String
indent n =
    replicate (2*n) ' '


printTree :: ToTree a => a -> IO ()
printTree =
    putStr . unlines . indentTree 1 . toTree


writeTree :: ToTree a => FilePath -> a -> IO ()
writeTree path =
    writeFile path . unlines . indentTree 0 . toTree


printParseOut :: FilePath -> IO ()
printParseOut target = do
    text <- readFile target
    case Parsec.parse parse target text of
        Left err  -> putStrLn $ show err
        Right out -> do
            putStrLn $ "• AST for " ++ (takeFileName target) ++ " •"
            printTree out


writeParseOut :: FilePath -> FilePath -> IO ()
writeParseOut target outputDirectory = do
    clearResult errPath
    clearResult outPath
    text <- readFile target
    case Parsec.parse parse target text of
        Left err  -> writeFile errPath $ show err
        Right out -> writeTree outPath out
  where targetName = reverse $ drop 4 $ reverse $ takeFileName target
        result  = outputDirectory ++ targetName
        errPath = result ++ ".ast.err"
        outPath = result ++ ".ast.out"



-- ERRORS


failStartsWith :: String -> String -> Token -> Parser a
failStartsWith production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \, but I am seeing \"" ++ (show actual) ++ "\""


failStartsWithOrEmpty :: String -> String -> Token -> Parser a
failStartsWithOrEmpty production expected actual =
    Parsec.unexpected $ ": A " ++ production ++ " must start with the \"" ++ expected ++ "\" keyword\
                        \ or be empty, but I am seeing \"" ++ (show actual) ++ "\""
