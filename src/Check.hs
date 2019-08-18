module Check where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe (listToMaybe)
import Data.Word (Word32)
import qualified Text.Parsec as Parsec

import Parser hiding (locals)

{-| This module implements semantic analysis to validate WebAssembly modules.

  The module takes ASTs produced by Parser.hs as input.

  Work has only begun on this module and much remains!

  This is an implementation of the WebAssembly spec released on January 10th,
  2019. Sections of the specification are referenced with § section marks
  throughout this module.
-}



-- CONTEXTS


data Context = Context
    { types :: [(MaybeIdent, FuncType)]
    , funcs :: [(MaybeIdent, TypeIndex)]
    -- add tables and mems
    , globals :: [(MaybeIdent, GlobalType)]
    , start :: Maybe ParserIdX
    , exports :: [Name]
    , valid :: Bool
    }


data LocalContext = LocalContext
    { locals :: [(MaybeIdent, ValType)]
    , operandStack :: [CheckValType]
    , controlStack :: [ControlFrame]
    }


data ControlFrame = ControlFrame
   { label :: MaybeIdent
   , labelTypes :: [CheckValType]
   , resultTypes :: [CheckValType]
   , height :: Int
   , unreachable :: Bool
   }

type Types = [(MaybeIdent, FuncType)]

type Funcs = [(MaybeIdent, TypeIndex)]

type TypeIndex = Word32

{- While checking we may have a ValType or Unknown on a polymorphic stack.
   A Option type is used where Just wraps a ValType and Nothing is Unknown.
-}
type CheckValType = Maybe ValType

type ContextState = StateT Context IO

type ValidationState = StateT (Context, LocalContext) IO


-- CHECK


check :: String -> IO ()
check filepath = do
    text <- readFile filepath
    case Parsec.parse parse filepath text of
        Left err  ->
            putStrLn $ show err
        Right ast ->
            case ast of
                Module maybeId components -> do
                    putStrLn "① Checking import order..."
                    if checkImportOrder components then do
                        liftIO $ putStrLn "② Adding types to the context..."
                        typedContext <- execStateT (registerTypes components) emptyContext
                        liftIO $ putStrLn "③ Adding imports, funcs, and globals to the context..."
                        context <- execStateT (registerComponents components) typedContext
                        if valid context then do
                            putStrLn $ show context
                            putStrLn "④ Checking func bodies, starts and exports..."
                            checkedContext <- execStateT (check_ components) context
                            if valid checkedContext then
                                putStrLn "✓ Module is valid"
                            else
                                putStrLn "🗙 Invalid module"
                        else
                            putStrLn "🗙 Invalid module"
                        return ()
                    else do
                        printImportOrderError
                        putStrLn "🗙 Invalid module"
                        return ()
                  where
                       emptyContext = Context { types = [], funcs = [], globals = [], start = Nothing, exports = [], valid = True }



-- CHECK IMPORT ORDER

{-| Imports must come before funcs, tables, memories or globals in the module.
-}


checkImportOrder :: Components ParserIdX -> Bool
checkImportOrder components =
    fst $ foldl importsFirst (True, True) components


importsFirst :: (Bool, Bool) -> Component ParserIdX -> (Bool, Bool)
importsFirst (inOrder, stillImporting) component =
    case component of
        Import _ _ _ -> (stillImporting, stillImporting)
        Func _ _ _ _ -> (inOrder, False)
        Global _ _ _ -> (inOrder, False)
        -- add tables and mems
        _            -> (inOrder, stillImporting)



-- CONTEXT PREPASSES

{-| Build the global context

  The registerTypes pass collects types.
  The registerComponents pass collects imports, funcs, globals, and exports. Any
  InlineTypes that do not exist by some other name in the types context are added in
  this pass.
-}


registerTypes :: Components ParserIdX -> ContextState ()
registerTypes components =
    mapM_ registerType components


registerType :: Component ParserIdX -> ContextState ()
registerType component = do
     context <- get
     case component of
         Type maybeId functype -> do
             printStep component
             updatedTypes <- addContextEntry "type" maybeId functype (types context)
             put (context { types = updatedTypes })
         _ -> return ()


registerComponents :: Components ParserIdX -> ContextState ()
registerComponents components =
     mapM_ registerComponent components


registerComponent :: Component ParserIdX -> ContextState ()
registerComponent component =
    case component of
        Type maybeId functype ->
            return ()
        Import _ _ importdesc -> do
            printStep component
            registerImport importdesc
        Func maybeId typeuse _ _ -> do
            printStep component
            registerFunc maybeId typeuse
        Start _ ->
            return ()
        Global maybeId globaltype _ -> do
            context <- get
            updatedGlobals <- addContextEntry "global" maybeId globaltype (globals context)
            put (context { globals = updatedGlobals })
        Export _ _ ->
            return ()


registerImport :: ImportDescription ParserIdX -> ContextState ()
registerImport importdesc = do
    context <- get
    case importdesc of
        FuncImport maybeId typeuse ->
            registerFunc maybeId typeuse
        -- add tables and mems here
        GlobalImport maybeId globaltype -> do
            updatedGlobals <- addContextEntry "global" maybeId globaltype (globals context)
            put (context { globals = updatedGlobals })


registerFunc :: MaybeIdent -> TypeUse ParserIdX -> ContextState ()
registerFunc maybeId typeuse = do
    context <- get
    case typeuse of
        TypeUse idx -> do
            maybeIndex <- checkTypeUse idx
            case maybeIndex of
                Just typeIndex -> do
                    updatedFuncs <- addContextEntry "func" maybeId typeIndex (funcs context)
                    put (context { funcs = updatedFuncs })
                Nothing -> return ()
        TypeUseWithDeclarations idx params results -> do
            maybeIndex <- checkTypeUseWithDeclarations idx params results
            case maybeIndex of
                Just typeIndex -> do
                    updatedFuncs <- addContextEntry "func" maybeId typeIndex (funcs context)
                    put (context { funcs = updatedFuncs })
                Nothing -> return ()
        InlineType params results -> do
            maybeIndex <- return $ lookupType (FuncType params results) (types context)
            case maybeIndex of
                Just typeIndex -> do
                    updatedFuncs <- addContextEntry "func" maybeId typeIndex (funcs context)
                    put (context { funcs = updatedFuncs })
                Nothing -> do
                    updatedTypes <- addContextEntry "type" Nothing (FuncType params results) (types context)
                    let maybeIndex = lookupType (FuncType params results) updatedTypes
                    case maybeIndex of
                        Just typeIndex -> do
                            updatedFuncs <- addContextEntry "func" maybeId typeIndex (funcs context)
                            put (context { types = updatedTypes, funcs = updatedFuncs })
                        Nothing ->
                            fail "I just added that type. This must be a compiler bug."



{-| Add a context entry

   Each context entry is a tuple with maybe and ident and a type definition or reference (a typedref).
   The position of each entry is its index.
-}

addContextEntry :: String -> MaybeIdent -> a -> [(MaybeIdent, a)] -> ContextState [(MaybeIdent, a)]
addContextEntry indexSpaceName maybeId typedref indexSpace =
    case maybeId of
        Just id ->
            if uniqueIdentifier id indexSpace then
                return (indexSpace ++ [(maybeId, typedref)])
            else do
                liftIO $ printError (IdAlreadyDefined id indexSpaceName)
                return indexSpace
        Nothing -> return (indexSpace ++ [(Nothing, typedref)])


uniqueIdentifier :: Ident -> [(MaybeIdent,a)] -> Bool
uniqueIdentifier id indexSpace =
    null [ j | (Just j, _) <- indexSpace, j == id ]



-- TYPEUSE


checkTypeUse :: ParserIdX -> ContextState (Maybe TypeIndex)
checkTypeUse idx = do
    context <- get
    case lookupByIdX idx (types context) of
        Just _ ->
            case idx of
                Left n   -> return $ Just n
                Right id -> return $ resolveId (Just id) (types context)
        Nothing -> do
            liftIO $ printError (MissingType idx)
            put (context { valid = False })
            return Nothing


checkTypeUseWithDeclarations :: ParserIdX -> Params -> Results -> ContextState (Maybe TypeIndex)
checkTypeUseWithDeclarations idx params results = do
    context <- get
    case lookupByIdX idx (types context) of
        Just functype ->
            case functype of
                FuncType contextParams contextResults -> do
                    paramChecks <- return $ checkParams contextParams params
                    resultChecks <- return $ checkResults contextResults results
                    if (and paramChecks) && (and resultChecks) then
                        case idx of
                            Left n   -> return $ Just n
                            Right id -> return $ resolveId (Just id) (types context)
                    else do
                        liftIO $ printError (TypeDeclMismatch idx)
                        put (context { valid = False })
                        return Nothing
        Nothing -> do
            liftIO $ printError (MissingType idx)
            put (context { valid = False })
            return Nothing



-- PARAMS AND RESULTS


checkParams :: Params -> Params -> [Bool]
checkParams contextParams referenceParams =
    map checkParam $ zip contextParams referenceParams


checkParam :: (Param, Param) -> Bool
checkParam (Param _ contextValtype, Param _ referenceValtype) =
    contextValtype == referenceValtype

checkResults :: Results -> Results -> [Bool]
checkResults contextResults referenceResults =
    map checkResult $ zip contextResults referenceResults


checkResult :: (Result, Result) -> Bool
checkResult (Result contextValtype, Result referenceValtype) =
    contextValtype == referenceValtype


-- CHECK

{-| Check func bodies, start and exports

  Check func bodies.
  Check that Start and Exports reference a valid func indices.
-}

check_ :: Components ParserIdX -> ContextState ()
check_ components =
    mapM_ checkFuncs components


checkFuncs :: Component ParserIdX -> ContextState ()
checkFuncs component = do
    context <- get
    case component of
        Type _ _ ->
            return ()
        Import _ _ _ ->
            return ()
        Func maybeIdent typeuse locals instructions -> do
            printFuncStep component
            checkFunc maybeIdent typeuse locals instructions
        Start idx -> do
            printStep component
            checkStart idx
        Global maybeIdent globaltype instructions -> do
            printGlobalStep component
            checkGlobal maybeIdent globaltype instructions
        Export name exportdesc -> do
            printStep component
            checkExport name exportdesc


checkFunc :: MaybeIdent -> TypeUse ParserIdX -> Locals -> Instructions ParserIdX -> ContextState ()
checkFunc maybeIdent typeuse locals instructions = do
    context <- get
    bodyLocals <- makeBodyLocals typeuse locals
    results <- lookupResults typeuse
    let controlFrame = ControlFrame {label = maybeIdent, labelTypes = results, resultTypes = results , height = 0, unreachable = False}
        emptyLocalContext = LocalContext { locals = bodyLocals, operandStack = [], controlStack = [controlFrame] }
    (_, (checkedContext, _)) <- lift $ runStateT (checkBlock instructions) (context, emptyLocalContext)
    put (context { valid = valid checkedContext })
    return ()


checkStart :: ParserIdX -> ContextState ()
checkStart idx = do
    context <- get
    if null (start context) then
        case lookupByIdX idx (funcs context) of
            Just _ -> do
                put (context { start = Just idx })
                return ()
            Nothing -> do
                put (context { valid = False })
                liftIO $ printError (MissingType idx)
                return ()
    else do
        put (context { valid = False })
        liftIO $ printError MultipleStart
        return ()


checkGlobal :: MaybeIdent -> GlobalType -> Instructions ParserIdX -> ContextState ()
checkGlobal maybeIdent globaltype instructions = do
    context <- get
    results <- return $ lookupGlobalResult globaltype
    let controlFrame = ControlFrame {label = maybeIdent, labelTypes = results, resultTypes = results , height = 0, unreachable = False}
        emptyLocalContext = LocalContext { locals = [], operandStack = [], controlStack = [controlFrame] }
    (_, (checkedContext, _)) <- lift $ runStateT (checkBlock instructions) (context, emptyLocalContext)
    put (context { valid = valid checkedContext })
    return ()


lookupGlobalResult :: GlobalType -> [CheckValType]
lookupGlobalResult globaltype =
    case globaltype of
        GlobalConst valtype -> 
            [Just valtype]
        GlobalVar valtype ->
            [Just valtype]


checkExport :: Name -> ExportDescription ParserIdX -> ContextState ()
checkExport name exportdesc = do
   context <- get
   if elem name (exports context) then do
       put (context { valid = False })
       liftIO $ printDuplicateExportError name
   else do
       put(context { exports = name : exports context })
       case exportdesc of
          FuncExport idx ->
              lookupExport idx (funcs context)
          GlobalExport idx ->
              lookupExport idx (globals context)


lookupExport :: ParserIdX -> [(MaybeIdent, a)] -> ContextState ()
lookupExport idx contextSpace = 
    case lookupByIdX idx contextSpace of
        Just _ ->
            return ()
        Nothing -> do
            context <- get
            put (context { valid = False })
            liftIO $ printError (MissingType idx)
            return ()


-- CHECK FUNC BODY


checkBlock :: Instructions ParserIdX -> ValidationState [CheckValType]
checkBlock instructions = do
    printLocalContext
    mapM_ checkInstructionStep instructions
    results <- popControlFrame
    pushOpds results
    printLocalContext
    return results


checkInstructionStep :: Instruction ParserIdX -> ValidationState ()
checkInstructionStep instruction = do
    checkInstruction instruction
    printInstructionStep instruction


checkInstruction :: Instruction ParserIdX -> ValidationState ()
checkInstruction instruction = do
    printBlockStep instruction
    (context, localContext) <- get
    case instruction of
        -- control instructions [§3.3.5]
        Block maybeIdent resultType instructions              -> do
            pushControlFrame maybeIdent (toLabelTypes resultType) (toLabelTypes resultType)
            checkBlock instructions
            return ()
        Loop maybeIdent resultType instructions               -> do
            pushControlFrame maybeIdent [] (toLabelTypes resultType)
            checkBlock instructions
            return ()
        Conditional maybeIdent resultType ifInstructions elseInstructions -> do
            popCheckOpd (Just I32)
            pushControlFrame maybeIdent (toLabelTypes resultType) (toLabelTypes resultType)
            results <- checkBlock ifInstructions
            if not $ null elseInstructions then do
              printElseStep instruction
              popCheckOpds results -- clear results from if arm
              pushControlFrame maybeIdent results results
              checkBlock elseInstructions
              return ()
            else
              return ()
        Unreachable                                           -> unreachable_
        Nop                                                   -> return ()
        Br idx                                                -> do
            maybeFrame <- lookupControlFrame idx
            case maybeFrame of
                Just frame -> do
                    popCheckOpds (labelTypes frame)
                    unreachable_
                Nothing -> do
                    liftIO $ printError (BrTargetNotFound idx)
                    fail ""
        BrIf idx                                              -> do
            maybeFrame <- lookupControlFrame idx
            case maybeFrame of
                Just frame -> do
                    popCheckOpd (Just I32)
                    popCheckOpds (labelTypes frame)
                    pushOpds (labelTypes frame)
                Nothing -> do
                    liftIO $ printError (BrTargetNotFound idx)
                    fail ""
        BrTable idxs idx                                      -> return ()
        Return                                                -> return ()
        Call idx                                              -> do
            functype <- lookupFunc idx
            case functype of
                Just (FuncType params results) -> do
                    popCheckOpds $ map paramToCheckValType params
                    pushOpds $ map resultToCheckValType results
                Nothing -> do
                    liftIO $ printError (FuncNotDefined idx)
                    fail ""
        CallIndirect typeuse                                  -> return ()

        -- parametric instructions [§3.3.2]
        Drop   -> do
            popOpd
            return ()
        Select -> do
            vt1 <- popOpd
            vt2 <- popCheckOpd vt1
            popCheckOpd (Just I32)
            pushOpd vt2
            return ()

        -- variable instructions [§3.3.3]
        LocalGet idx -> do
            locals <- return $ locals localContext
            checkLocalGet idx locals

        LocalSet idx  -> do
            locals <- return $ locals localContext
            checkLocalSet idx locals

        LocalTee idx  -> do
            locals <- return $ locals localContext
            checkLocalTee idx locals

        GlobalGet idx -> do
            globals <- return $ globals context
            checkGlobalGet idx globals

        GlobalSet idx -> do
            globals <- return $ globals context
            checkGlobalSet idx globals

        -- add memory instructions [§3.3.4]

        -- numeric instructions [§3.3.1]
        I32Const _            -> checkConstOp I32
        I64Const _            -> checkConstOp I64
        F32Const _            -> checkConstOp F32
        F64Const _            -> checkConstOp F64

        I32Clz                -> checkUnOp I32
        I32Ctz                -> checkUnOp I32
        I32Popcnt             -> checkUnOp I32
        I32Add                -> checkBinOp I32
        I32Sub                -> checkBinOp I32
        I32Mul                -> checkBinOp I32
        I32Div Signed         -> checkBinOp I32
        I32Div Unsigned       -> checkBinOp I32
        I32Rem Signed         -> checkBinOp I32
        I32Rem Unsigned       -> checkBinOp I32
        I32And                -> checkBinOp I32
        I32Or                 -> checkBinOp I32
        I32Xor                -> checkBinOp I32
        I32Shl                -> checkBinOp I32
        I32Shr Signed         -> checkBinOp I32
        I32Shr Unsigned       -> checkBinOp I32
        I32Rotl               -> checkBinOp I32
        I32Rotr               -> checkBinOp I32

        I64Clz                -> checkUnOp I64
        I64Ctz                -> checkUnOp I64
        I64Popcnt             -> checkUnOp I64
        I64Add                -> checkBinOp I64
        I64Sub                -> checkBinOp I64
        I64Mul                -> checkBinOp I64
        I64Div Signed         -> checkBinOp I64
        I64Div Unsigned       -> checkBinOp I64
        I64Rem Signed         -> checkBinOp I64
        I64Rem Unsigned       -> checkBinOp I64
        I64And                -> checkBinOp I64
        I64Or                 -> checkBinOp I64
        I64Xor                -> checkBinOp I64
        I64Shl                -> checkBinOp I64
        I64Shr Signed         -> checkBinOp I64
        I64Shr Unsigned       -> checkBinOp I64
        I64Rotl               -> checkBinOp I64
        I64Rotr               -> checkBinOp I64

        F32Abs                -> checkUnOp F32
        F32Neg                -> checkUnOp F32
        F32Sqrt               -> checkUnOp F32
        F32Ceil               -> checkUnOp F32
        F32Floor              -> checkUnOp F32
        F32Trunc              -> checkUnOp F32
        F32Nearest            -> checkUnOp F32
        F32Add                -> checkBinOp F32
        F32Sub                -> checkBinOp F32
        F32Mul                -> checkBinOp F32
        F32Div                -> checkBinOp F32
        F32Min                -> checkBinOp F32
        F32Max                -> checkBinOp F32
        F32Copysign           -> checkBinOp F32

        F64Abs                -> checkUnOp F64
        F64Neg                -> checkUnOp F64
        F64Sqrt               -> checkUnOp F64
        F64Ceil               -> checkUnOp F64
        F64Floor              -> checkUnOp F64
        F64Trunc              -> checkUnOp F64
        F64Nearest            -> checkUnOp F64
        F64Add                -> checkBinOp F64
        F64Sub                -> checkBinOp F64
        F64Mul                -> checkBinOp F64
        F64Div                -> checkBinOp F64
        F64Min                -> checkBinOp F64
        F64Max                -> checkBinOp F64
        F64Copysign           -> checkBinOp F64

        I32Eqz                -> checkTestOp I32
        I32Eq                 -> checkRelOp I32
        I32Ne                 -> checkRelOp I32
        I32Lt Signed          -> checkRelOp I32
        I32Lt Unsigned        -> checkRelOp I32
        I32Gt Signed          -> checkRelOp I32
        I32Gt Unsigned        -> checkRelOp I32
        I32Le Signed          -> checkRelOp I32
        I32Le Unsigned        -> checkRelOp I32
        I32Ge Signed          -> checkRelOp I32
        I32Ge Unsigned        -> checkRelOp I32

        I64Eqz                -> checkTestOp I64
        I64Eq                 -> checkRelOp I64
        I64Ne                 -> checkRelOp I64
        I64Lt Signed          -> checkRelOp I64
        I64Lt Unsigned        -> checkRelOp I64
        I64Gt Signed          -> checkRelOp I64
        I64Gt Unsigned        -> checkRelOp I64
        I64Le Signed          -> checkRelOp I64
        I64Le Unsigned        -> checkRelOp I64
        I64Ge Signed          -> checkRelOp I64
        I64Ge Unsigned        -> checkRelOp I64

        F32Eq                 -> checkRelOp F32
        F32Ne                 -> checkRelOp F32
        F32Lt                 -> checkRelOp F32
        F32Gt                 -> checkRelOp F32
        F32Le                 -> checkRelOp F32
        F32Ge                 -> checkRelOp F32

        F64Eq                 -> checkRelOp F64
        F64Ne                 -> checkRelOp F64
        F64Lt                 -> checkRelOp F64
        F64Gt                 -> checkRelOp F64
        F64Le                 -> checkRelOp F64
        F64Ge                 -> checkRelOp F64

        I32WrapI64             -> checkCvtOp I64 I32
        I32TruncF32 Signed     -> checkCvtOp F32 I32
        I32TruncF32 Unsigned   -> checkCvtOp F32 I32
        I32TruncF64 Signed     -> checkCvtOp F64 I32
        I32TruncF64 Unsigned   -> checkCvtOp F64 I32
        I64ExtendI32 Signed    -> checkCvtOp I32 I64
        I64ExtendI32 Unsigned  -> checkCvtOp I32 I64
        I64TruncF32 Signed     -> checkCvtOp F32 I64
        I64TruncF32 Unsigned   -> checkCvtOp F32 I64
        I64TruncF64 Signed     -> checkCvtOp F64 I64
        I64TruncF64 Unsigned   -> checkCvtOp F64 I64
        F32ConvertI32 Signed   -> checkCvtOp I32 F32
        F32ConvertI32 Unsigned -> checkCvtOp I32 F32
        F32ConvertI64 Signed   -> checkCvtOp I64 F32
        F32ConvertI64 Unsigned -> checkCvtOp I64 F32
        F32DemoteF64           -> checkCvtOp F64 F32
        F64ConvertI32 Signed   -> checkCvtOp I32 F64
        F64ConvertI32 Unsigned -> checkCvtOp I32 F64
        F64ConvertI64 Signed   -> checkCvtOp I64 F64
        F64ConvertI64 Unsigned -> checkCvtOp I64 F64
        F64PromoteF32          -> checkCvtOp F32 F64
        I32ReinterpretF32      -> checkCvtOp F32 I32
        I64ReinterpretF64      -> checkCvtOp F64 I64
        F32ReinterpretI32      -> checkCvtOp I32 F32
        F64ReinterpretI64      -> checkCvtOp I64 F64


toLabelTypes :: ResultType -> [CheckValType]
toLabelTypes resultType =
    case resultType of
        Just (Result valtype) ->
            [Just valtype]
        Nothing ->
            []


paramToCheckValType :: Param -> CheckValType
paramToCheckValType (Param _ vt) =
    Just vt


resultToCheckType :: Result -> CheckValType
resultToCheckType (Result vt) =
    Just vt



-- NUMERIC INSTRUCTION CHECKS


checkConstOp :: ValType -> ValidationState ()
checkConstOp vt = do
    pushOpd (Just vt)
    return ()


checkBinOp :: ValType -> ValidationState ()
checkBinOp vt = do
    popCheckOpd (Just vt)
    popCheckOpd (Just vt)
    pushOpd (Just vt)
    return ()


checkUnOp :: ValType -> ValidationState ()
checkUnOp vt = do
    popCheckOpd (Just vt)
    pushOpd (Just vt)
    return ()


checkTestOp :: ValType -> ValidationState ()
checkTestOp vt = do
    popCheckOpd (Just vt)
    pushOpd (Just I32)
    return ()


checkRelOp :: ValType -> ValidationState ()
checkRelOp vt = do
    popCheckOpd (Just vt)
    popCheckOpd (Just vt)
    pushOpd (Just I32)
    return ()


checkCvtOp :: ValType -> ValType -> ValidationState ()
checkCvtOp vt1 vt2 = do
    popCheckOpd (Just vt1)
    pushOpd (Just vt2)
    return ()



-- VARIABLE INSTRUCTION CHECKS


checkLocalGet :: ParserIdX -> [(MaybeIdent, ValType)] -> ValidationState ()
checkLocalGet idx locals = do
    maybeValType <- return $ lookupByIdX idx locals
    case maybeValType of
        Just vt -> do
            pushOpd (Just vt)
            return ()
        Nothing ->
            fail $ "🗙 Could not find local" ++ showIdX idx


checkLocalSet :: ParserIdX -> [(MaybeIdent, ValType)] -> ValidationState ()
checkLocalSet idx locals = do
    maybeValType <- return $ lookupByIdX idx locals
    case maybeValType of
        Just vt -> do
            popCheckOpd (Just vt)
            return ()
        Nothing ->
            fail $ "🗙 Could not find local" ++ showIdX idx


checkLocalTee :: ParserIdX -> [(MaybeIdent, ValType)] -> ValidationState ()
checkLocalTee idx locals = do
    maybeValType <- return $ lookupByIdX idx locals
    case maybeValType of
        Just vt -> do
            popCheckOpd (Just vt)
            pushOpd (Just vt)
            return ()
        Nothing ->
            fail $ "🗙 Could not find local" ++ showIdX idx


checkGlobalGet :: ParserIdX -> [(MaybeIdent, GlobalType)] -> ValidationState ()
checkGlobalGet idx globals = do
    maybeGlobalType <- return $ lookupByIdX idx globals
    case maybeGlobalType of
        Just gt -> do
            case gt of
                GlobalConst vt -> do
                    pushOpd (Just vt)
                    return ()
                GlobalVar vt -> do
                    pushOpd (Just vt)
                    return ()
        Nothing ->
            fail $ "🗙 Could not find global" ++ showIdX idx


checkGlobalSet :: ParserIdX -> [(MaybeIdent, GlobalType)] -> ValidationState ()
checkGlobalSet idx globals = do
    maybeGlobalType <- return $ lookupByIdX idx globals
    case maybeGlobalType of
        Just gt -> do
            case gt of
                GlobalConst vt -> do
                    fail "🗙 Cannot set a constant global"
                GlobalVar vt -> do
                    popCheckOpd (Just vt)
                    return ()
        Nothing ->
            fail $ "🗙 Could not find global" ++ showIdX idx




-- OPERAND AND CONTROL STACK MANIPULATION


popOpd :: ValidationState (CheckValType)
popOpd = do
    (context, localContext) <- get
    operandStack <- return $ operandStack localContext
    controlStack <- return $ controlStack localContext
    case safeHead controlStack of
        Right controlFrame ->
            if (length operandStack == height controlFrame && unreachable controlFrame) then
                return Nothing
            else if (length operandStack == height controlFrame) then do
                liftIO $ printError Underflow
                fail ""
            else
                case operandStack of
                    op:ops -> do
                        put (context, localContext { operandStack = ops })
                        return op
                    []     -> do
                        liftIO $ printError PopEmptyOps
                        fail ""
        Left err ->
            fail err


popCheckOpd :: CheckValType ->  ValidationState (CheckValType)
popCheckOpd expect = do
    actual <- popOpd
    if (actual == Nothing) then
        return expect
    else if (expect == Nothing) then
        return actual
    else
        if (actual /= expect) then do
            liftIO $ printError OperandMismatch
            fail ""
        else
            return actual


pushOpd :: CheckValType -> ValidationState ()
pushOpd checkValtype = do
    (context, localContext) <- get
    ops <- return $ operandStack localContext
    put (context, localContext { operandStack = checkValtype : ops })
    return ()


pushOpds :: [CheckValType] -> ValidationState ()
pushOpds opds =
    mapM_ pushOpd opds


popCheckOpds :: [CheckValType] -> ValidationState ()
popCheckOpds opds =
    mapM_ popCheckOpd opds


popControlFrame :: ValidationState [CheckValType]
popControlFrame = do
    (context, localContext) <- get
    controlStack <- return $ controlStack localContext
    case (listToMaybe controlStack) of
        Just frame -> do
            expectedResults <- return $ resultTypes frame
            popCheckOpds expectedResults
            (_, localContext) <- get
            operandStack <- return $ operandStack localContext
            if ((length operandStack) /= (height frame)) then do
                liftIO $ printError ExitHeightMismatch
                fail ""
            else
                case safeTail controlStack of
                    Right newStack -> do
                        put (context, localContext { controlStack = newStack })
                        return (resultTypes frame)
                    Left err       ->
                        fail err
        Nothing -> do
            liftIO $ printError PopEmptyFrames
            fail ""


pushControlFrame :: MaybeIdent -> [CheckValType] -> [CheckValType] -> ValidationState ()
pushControlFrame maybeIdent labels results = do
    (context, localContext) <- get
    frames <- return $ controlStack localContext
    opds <- return $ operandStack localContext
    let frame = ControlFrame { label = maybeIdent, labelTypes = labels, resultTypes = results, height = length opds, unreachable = False } in do
        put (context, localContext { controlStack = frame : frames})
        return ()


unreachable_ :: ValidationState ()
unreachable_ = do
    (context, localContext) <- get
    frames <- return $ controlStack localContext
    case safeHead frames of
        Right frame -> do
            opdStack <- return $ operandStack localContext
            resizedOpdStack <- return $ snd $ splitAt (length opdStack - height frame) opdStack
            case safeTail frames of
                Right controlStackTail -> do
                    put (context, localContext { operandStack = resizedOpdStack, controlStack = frame { unreachable = True } : controlStackTail})
                Left err ->
                    fail err
        Left err ->
           fail err



safeHead :: [a] -> Either String a
safeHead as =
    case as of
        a:as -> Right a
        []   -> Left "🗙 Cannot peek at the top of an empty stack"


safeTail :: [a] -> Either String [a]
safeTail as =
    case as of
        a:as -> Right as
        []   -> Left "🗙 Cannot delete the top of an empty stack"



-- ASSEMBLE FUNC LOCALS


makeBodyLocals :: TypeUse ParserIdX -> Locals -> ContextState [(MaybeIdent, ValType)]
makeBodyLocals typeuse locals = do
    maybeParams <- lookupParams typeuse
    case maybeParams of
        Just params -> do
            ps <- return $ map paramToBodyLocal params
            ls <- return $ map localToBodyLocal locals
            return (ps ++ ls)
        Nothing -> do
            liftIO $ putStrLn "Missing params because missing function - but we already know it is there!"
            return []


-- toBodyLocal :: a -> (MaybeIdent, ValType)
-- toBodyLocal (Param maybeIdent valtype) = (maybeIdent, valtype)
-- toBodyLocal (Local maybeIdent valtype) = (maybeIdent, valtype)

paramToBodyLocal :: Param -> (MaybeIdent, ValType)
paramToBodyLocal (Param maybeIdent valtype) =
    (maybeIdent, valtype)


localToBodyLocal :: Local -> (MaybeIdent, ValType)
localToBodyLocal (Local maybeIdent valtype) =
    (maybeIdent, valtype)



-- LOOKUP


lookupByIdX :: ParserIdX -> [(MaybeIdent, a)] -> Maybe a
lookupByIdX idx entries =
    case idx of
        Left n   -> lookupByIndex n entries
        Right id -> lookupById id entries


lookupByIndex :: Word32 -> [(MaybeIdent, a)] -> Maybe a
lookupByIndex n ts =
    case ts of
        t:ts ->
            case n of
                0 -> Just $ snd t
                _ -> lookupByIndex (n-1) ts
        []   ->
            Nothing


lookupById :: Ident -> [(MaybeIdent, a)] -> Maybe a
lookupById id ts =
    case filter (\t -> fst t == Just id) ts of
        t:ts -> Just $ snd t
        []   -> Nothing


resolveId :: MaybeIdent -> [(MaybeIdent, a)] -> Maybe TypeIndex
resolveId id xs =
    listToMaybe $ [ i | (x,i) <- zip xs [0..], id == fst x ]


lookupType :: (Eq a) => a -> [(MaybeIdent, a)] -> Maybe TypeIndex
lookupType type_ xs =
    listToMaybe $ [ i | (x,i) <- zip xs [0..], type_ == snd x ]


lookupParams :: TypeUse ParserIdX -> ContextState (Maybe Params)
lookupParams typeuse =
    case typeuse of
        TypeUse idx -> do
            context <- get
            maybeFuncType <- return $ lookupByIdX idx (types context)
            case maybeFuncType of
                Just functype ->
                    case functype of
                        FuncType params _ ->
                            return (Just params)
                Nothing -> do
                    liftIO $ printError (MissingType idx)
                    return Nothing
        TypeUseWithDeclarations idx params results ->
            return (Just params)
        InlineType params results ->
            return (Just params)


lookupResults :: TypeUse ParserIdX -> ContextState [CheckValType]
lookupResults typeuse =
    case typeuse of
        TypeUse idx -> do
            context <- get
            maybeFuncType <- return $ lookupByIdX idx (types context)
            case maybeFuncType of
                Just functype ->
                    case functype of
                        FuncType _ results ->
                            return $ map resultToCheckValType results
                Nothing -> do
                    liftIO $ printError (MissingType idx)
                    return []
        TypeUseWithDeclarations idx params results ->
            return $ map resultToCheckValType results
        InlineType params results ->
            return $ map resultToCheckValType results


resultToCheckValType :: Result -> CheckValType
resultToCheckValType (Result valtype) =
    Just valtype


lookupFunc :: ParserIdX -> ValidationState (Maybe FuncType)
lookupFunc idx = do
    (context, _) <- get
    case lookupByIdX idx (funcs context) of
        Just typeIndex ->
            case lookupByIndex typeIndex (types context) of
                Just functype -> do
                    return (Just functype)
                Nothing ->
                    fail "type for func not found"
        Nothing                        ->
            return Nothing


lookupControlFrame :: ParserIdX -> ValidationState (Maybe ControlFrame)
lookupControlFrame idx = do
    (_, localContext) <- get
    controlStack <- return $ controlStack localContext
    case idx of
        Left n   ->
            if fromIntegral n <= length controlStack then do
                frame <- return $ listToMaybe $
                    [ f | (f,i) <- zip controlStack [0..], i == fromIntegral n]
                return frame
            else
                return Nothing
        Right id -> do
            frame <- return $ listToMaybe $
                [ f | (f,i) <- zip controlStack [0..], (Just id) == label f]
            return frame



-- SHOW


instance Show Context where
    show (Context types funcs globals start exports valid) = "∙context∙" ++ "\n"
        ++ indent 1 ++ "types\n" ++ (concat $ map showType types)
        ++ indent 1 ++ "funcs\n" ++ (concat $ map showTypeRef funcs)
        ++ indent 1 ++ "globals\n" ++ (concat $ map showType globals)
        ++ indent 1 ++ "start" ++ showMaybeIdX start ++ "\n"
        ++ indent 1 ++ "exports\n" ++ (concat $ map showName exports) ++ "\n"
        ++ indent 1 ++ "valid " ++ show valid
        ++ "\n"

instance Show LocalContext where
    show (LocalContext locals operandStack controlStack) = "∙local context∙" ++ "\n"
        ++ indent 1 ++ "locals\n" ++ (concat $ map showType locals)
        ++ indent 1 ++ "operand stack\n"
        ++ indent 3 ++ "[" ++ (concat $ map showCheckValType operandStack) ++ " ]" ++ "\n"
        ++ indent 1 ++ "control stack\n" ++ (concat $ map showControlFrame $ zip [0..] controlStack)
        -- ++ "\n"

instance Show ControlFrame where
    show (ControlFrame label labelTypes resultTypes height unreachable) =
        "label" ++ showMaybeId label ++ "\n"
            ++ indent 5 ++ "label types [" ++ (concat $ map showCheckValType labelTypes) ++ " ]\n"
            ++ indent 5 ++ "result types [" ++ (concat $ map showResultType resultTypes) ++ " ]\n"
            ++ indent 5 ++ "entry height " ++ show height ++ "\n"
            ++ indent 5 ++ "unreachable " ++ show unreachable
            ++ "\n"

instance Show FuncType where
    show (FuncType params results) =
        concat $ (map (\p -> p ++ " ->") $ showParams params) ++ showResults results

instance Show Param where
    show (Param maybeId valtype) =
        showMaybeId maybeId ++ show valtype

instance Show Result where
    show (Result valtype) =
        show valtype

instance Show GlobalType where
    show (GlobalVar valtype) =
        " mut" ++ show valtype
    show (GlobalConst valtype) =
        " const" ++ show valtype

showType :: Show a => (MaybeIdent, a) -> String
showType (maybeId, typedef) =
    indent 3
        ++ case maybeId of
               Just id -> show id ++ " :" ++ show typedef ++ "\n"
               Nothing -> "_ :" ++ show typedef ++ "\n"


showTypeRef :: (MaybeIdent, TypeIndex) -> String
showTypeRef (maybeId, typeref) =
   indent 3
        ++ case maybeId of
               Just id -> show id ++ " : {" ++ show typeref ++ "}\n"
               Nothing -> "_ : {" ++ show typeref ++ "}\n"


showParams :: [Param] -> [String]
showParams params =
    case params of
        p:ps -> map show (p:ps)
        _    -> [" ()"]


showResults :: [Result] -> [String]
showResults results =
    case results of
        r:rs -> map show (r:rs)
        _    -> [" ()"]


showMaybeIdX :: Maybe ParserIdX -> String
showMaybeIdX maybeIdX =
    case maybeIdX of
        Just id -> showIdX id
        Nothing -> ""


showCheckValType :: CheckValType -> String
showCheckValType maybeValtype =
    case maybeValtype of
        Just valtype -> show valtype
        Nothing -> " Unknown"


showResultType :: CheckValType -> String
showResultType maybeValtype =
    case maybeValtype of
        Just valtype -> show valtype
        Nothing -> " ()"


showControlFrame :: (Int, ControlFrame) -> String
showControlFrame (depth, controlFrame) =
    indent 3 ++ show depth ++ " | " ++ show controlFrame


showInstructionContext :: LocalContext -> String
showInstructionContext localContext =
    case localContext of
        LocalContext locals operandStack _ ->
            indent 1 ++ "locals\n" ++ (concat $ map showType locals)
                ++ indent 1 ++ "operand stack\n"
                ++ indent 3 ++ "[" ++ (concat $ map showCheckValType operandStack) ++ " ]"
                ++ "\n"


showName :: Name -> String
showName name =
    indent 3 ++ show name ++ "\n"



-- PRINT


printStep :: Component ParserIdX -> ContextState ()
printStep component = do
    context <- get
    liftIO $ putStrLn $ show context
    liftIO $ putStrLn "🞅∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙"
    liftIO $ putStrLn "∙component∙"
    liftIO $ printTree component
    liftIO $ putStr "\n"


printFuncStep :: Component ParserIdX -> ContextState ()
printFuncStep component = do
    liftIO $ putStrLn "λ••••••••••••••••"
    liftIO $ putStrLn $ "checking func body:"
    liftIO $ printTree component
    liftIO $ putStr "\n"

  
printGlobalStep :: Component ParserIdX -> ContextState ()
printGlobalStep component = do
    liftIO $ putStrLn "🞊••••••••••••••••"
    liftIO $ putStrLn $ "checking global initializer:"
    liftIO $ printTree component
    liftIO $ putStr "\n"


printLocalContext :: ValidationState ()
printLocalContext = do
    printBlockMark '🞎'
    (_, localContext) <- get
    liftIO $ putStrLn $ show localContext ++ "\n"


printBlockStep :: Instruction ParserIdX -> ValidationState ()
printBlockStep instruction = do
    case instruction of
        Block _ _ _          -> do
            printBlockMark '🞔'
            liftIO $ putStrLn $ "checking block:"
            liftIO $ printTree instruction
            liftIO $ putStr "\n"
        Loop _ _ _           -> do
            printBlockMark '🞔'
            liftIO $ putStrLn $ "checking loop:"
            liftIO $ printTree instruction
            liftIO $ putStr "\n"
        Conditional _ _ _ _  -> do
            printBlockMark '🞔'
            liftIO $ putStrLn $ "checking conditional:"
            liftIO $ printTree instruction
            liftIO $ putStr "\n"
        _                    ->
            return ()
 

printElseStep :: Instruction ParserIdX -> ValidationState ()
printElseStep instruction = do
    printBlockMark '🞔'
    liftIO $ putStrLn $ "checking else arm:"
    liftIO $ printTree instruction
    liftIO $ putStr "\n"


printInstructionStep :: Instruction ParserIdX -> ValidationState ()
printInstructionStep instruction =
    case instruction of
        Block _ _ _          -> return ()
        Loop _ _ _           -> return ()
        Conditional _ _ _ _  -> return ()
        _                    -> do
            printBlockMark '⬚'
            liftIO $ printTree instruction
            (_, localContext) <- get
            liftIO $ putStrLn $ "\n" ++ showInstructionContext localContext ++ "\n"


printBlockMark :: Char -> ValidationState ()
printBlockMark blockChar = do
    (_, localContext) <- get
    depth <- return $ length $ controlStack localContext
    liftIO $ putStrLn $ (replicate depth '∙') ++ [blockChar] ++ (replicate (16 - depth) '∙')



-- ERRORS


data Error = MissingType ParserIdX
           | MissingFunc ParserIdX
           | TypeDeclMismatch ParserIdX
           | MultipleStart
           | ImportOrder
           | IdAlreadyDefined Ident String
           | Underflow
           | PopEmptyOps
           | PopEmptyFrames
           | OperandMismatch
           | ExitHeightMismatch
           | FuncNotDefined ParserIdX
           | BrTargetNotFound ParserIdX


printError :: Error -> IO ()
printError =
    putStrLn . ("🗙 " ++) . showError


showError :: Error -> String
showError error =
    case error of
        MissingType idx ->
            "A type with the identifier" ++ (showIdX idx) ++ " could not be found\n"
        MissingFunc idx ->
            "A func with the identifier" ++ (showIdX idx) ++ " could not be found\n"
        TypeDeclMismatch idx ->
            "The inline declarations used in this type use do not match the reference type" ++ (showIdX idx) ++ "\n"
        MultipleStart ->
            "A start component has already registered an entry point to this module"
        ImportOrder ->
            "Imports must come before any funcs, tables, memories, or globals in a valid module"
        IdAlreadyDefined id indexSpaceName ->
            "The identifier " ++ show id
                ++ " already defined in the " ++ indexSpaceName ++ " index space"
        Underflow ->
            "Pop operation undeflows the current block"
        PopEmptyOps ->
            "Cannot pop an empty operand stack"
        PopEmptyFrames ->
            "Attempt to pop a control frame but there are none"
        OperandMismatch ->
            "Actual operand does not match expected operand"
        ExitHeightMismatch ->
            "The operand stack was not returned to its initial height at the end of this block"
        FuncNotDefined idx ->
            "The func" ++ showIdX idx ++ " is not defined"
        BrTargetNotFound idx ->
            "Could not find a frame with idx" ++ showIdX idx ++ " to br to"


printImportOrderError :: IO ()
printImportOrderError =
    putStrLn $ "🗙 Imports must come before any funcs, tables, memories, or globals in a valid module"


printDuplicateExportError :: Name -> IO ()
printDuplicateExportError name =
    putStrLn $ "🗙 Export names must be unique and" ++ show name ++ " has already been used"
