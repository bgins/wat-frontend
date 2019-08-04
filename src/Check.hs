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
    , valid :: Bool
    }


data LocalContext = LocalContext
    { locals :: [(MaybeIdent, ValType)]
    , operandStack :: [Maybe ValType] -- Nothing represents Unknown
    , controlStack :: [ControlFrame]
    -- , controlStack :: [(MaybeIdent, ControlFrame)]
    }


data ControlFrame = ControlFrame
   { labelTypes :: [ValType]  -- [ResultType] ??
   , resultType :: ResultType
   , height :: Int
   , unreachable :: Bool
   }

  -- it seems like we should track labels but we don't?
  -- , labels :: [(MaybeIdent, ResultType)]
  -- , return_ :: (MaybeIdent, ResultType)   -- Maybe return_?

type Types = [(MaybeIdent, FuncType)]

type TypeIndex = Word32

type Funcs = [(MaybeIdent, TypeIndex)]


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
                        context <- execStateT (makeContext components) emptyContext
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
                       emptyContext = Context { types = [], funcs = [], globals = [], start = Nothing , valid = True }



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



-- CONTEXT PREPASS

{-| Build the global context

  The first pass collects types.
  The second pass collects imports, funcs, globals, and exports. Any InlineTypes that
  do not exist by some other name in the context are added in this pass.
-}


-- STP: move mapM calls to check function and print there?
makeContext :: Components ParserIdX -> ContextState ()
makeContext components = do
    liftIO $ putStrLn "② Adding types to the context..."
    mapM_ registerType components
    liftIO $ putStrLn "③ Adding imports, funcs, and globals to the context..."
    mapM_ registerComponent components


registerType :: Component ParserIdX -> ContextState ()
registerType component = do
     context <- get
     case component of
         Type maybeId functype -> do
             printStep component
             updatedTypes <- addContextEntry "type" maybeId functype (types context)
             put (context { types = updatedTypes })
         _ -> return ()


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
        -- add table, memory, and global imports here


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
        Func _ typeuse locals instructions -> do
            printFuncStep component
            checkFunc typeuse locals instructions
            return ()
        Start idx -> do
            printStep component
            checkStart idx
        Global maybeId globaltype _ ->
            -- check function expressions
            return ()
        Export _ exportdesc -> do
            printStep component
            checkExport exportdesc


checkFunc :: TypeUse ParserIdX -> Locals -> Instructions ParserIdX -> ContextState ()
checkFunc typeuse locals instructions = do
    context <- get
    bodyLocals <- makeBodyLocals typeuse locals
    result <- lookupResult typeuse
    let controlFrame = ControlFrame {labelTypes = [], resultType = result , height = 0, unreachable = False}
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


-- TODO: make sure export names are unique
checkExport :: ExportDescription ParserIdX -> ContextState ()
checkExport exportdesc = do
    context <- get
    case exportdesc of
       FuncExport idx ->
           case lookupByIdX idx (funcs context) of
               Just _ ->
                   return ()
               Nothing -> do
                   put (context { valid = False })
                   liftIO $ printError (MissingType idx)
                   return ()



-- CHECK FUNC BODY


checkBlock :: Instructions ParserIdX -> ValidationState ()
checkBlock instructions = do
    printBlockEntry
    mapM_ checkInstructionStep instructions
    popControlFrame
    printBlockExit
    return ()


checkInstructionStep :: Instruction ParserIdX -> ValidationState ()
checkInstructionStep instruction = do
    checkInstruction instruction
    printInstructionStep instruction


checkInstruction :: Instruction ParserIdX -> ValidationState ()
checkInstruction instruction = do
    (context, localContext) <- get
    case instruction of
        -- control instructions [§3.3.5]
        Block maybeIdent resultType instructions              -> return ()
        Loop maybeIdent resultType instructions               -> return ()
        Conditional maybeIdent resultType ifInstrs elseInstrs -> return ()
        Unreachable                                           -> return ()
        Nop                                                   -> return ()
        Br idx                                                -> return ()
        BrIf idx                                              -> return ()
        BrTable idxs idx                                      -> return ()
        Return                                                -> return ()
        Call idx                                              -> return ()
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
            maybeValtype <- return $ lookupByIdX idx locals
            case maybeValtype of
                Just vt -> do
                    pushOpd (Just vt)
                    return ()
                Nothing ->
                    fail $ "🗙 Could not find local" ++ show idx

        LocalSet idx  -> return ()
        LocalTee idx  -> return ()
        GlobalGet idx -> return ()
        GlobalSet idx -> return ()

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



-- OPERAND AND CONTROL STACK MANIPULATION


popOpd :: ValidationState (Maybe ValType)
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

popCheckOpd :: Maybe ValType ->  ValidationState (Maybe ValType)
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


pushOpd :: Maybe ValType -> ValidationState ()
pushOpd maybeValtype = do
    (context, localContext) <- get
    ops <- return $ operandStack localContext
    put (context, localContext { operandStack = maybeValtype : ops })
    return ()


popControlFrame :: ValidationState (Maybe ValType)
popControlFrame = do
    (context, localContext) <- get
    controlStack <- return $ controlStack localContext
    case (listToMaybe controlStack) of
        Just frame -> do
            maybeValtype <- return $ resultTypeToMaybe $ resultType frame
            result <- maybePopStack maybeValtype
            (_, localContext) <- get
            operandStack <- return $ operandStack localContext
            if ((length operandStack) /= (height frame)) then do
                liftIO $ printError ExitHeightMismatch
                fail ""
            else
                case safeTail controlStack of
                    Right newStack -> do
                        put (context, localContext { controlStack = newStack })
                        return result
                        -- algorithm suggests returning this, but why did we check?
                        -- return (resultType frame)
                    Left err       ->
                        fail err
        Nothing -> do
            liftIO $ printError PopEmptyFrames
            fail ""


maybePopStack :: Maybe ValType -> ValidationState (Maybe ValType)
maybePopStack maybeResult =
    case maybeResult of
       Just valtype -> do
           result <- popCheckOpd (Just valtype)
           return (Just valtype)
       Nothing ->
           return Nothing


pushControlFrame :: [ValType] -> ResultType -> ValidationState ()
pushControlFrame labels result = do
    (context, localContext) <- get
    frames <- return $ controlStack localContext
    ops <- return $ operandStack localContext
    let frame = ControlFrame { labelTypes = labels, resultType = result, height = length ops, unreachable = False } in do
        put (context, localContext { controlStack = frame : frames})
        return ()


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


resultTypeToMaybe :: ResultType -> Maybe ValType
resultTypeToMaybe resultType =
    case resultType of
        Just result ->
            case result of
                Result valtype -> Just valtype
        Nothing -> Nothing




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


lookupResult :: TypeUse ParserIdX -> ContextState ResultType
lookupResult typeuse =
    case typeuse of
        TypeUse idx -> do
            context <- get
            maybeFuncType <- return $ lookupByIdX idx (types context)
            case maybeFuncType of
                Just functype ->
                    case functype of
                        FuncType _ results ->
                            return (listToMaybe results)
                Nothing -> do
                    liftIO $ printError (MissingType idx)
                    return Nothing
        TypeUseWithDeclarations idx params results ->
            return (listToMaybe results)
        InlineType params results ->
            return (listToMaybe results)


-- SHOW


instance Show Context where
    show (Context types funcs globals start valid) = "∙context∙" ++ "\n"
        ++ indent 1 ++ "types\n" ++ (concat $ map showType types)
        ++ indent 1 ++ "funcs\n" ++ (concat $ map showTypeRef funcs)
        ++ indent 1 ++ "globals\n" ++ (concat $ map showType globals)
        ++ indent 1 ++ "start" ++ showMaybeIdX start ++ "\n"
        ++ indent 1 ++ "valid " ++ show valid
        ++ "\n"

instance Show LocalContext where
    show (LocalContext locals operandStack controlStack) = "∙local context∙" ++ "\n"
        ++ indent 1 ++ "locals\n" ++ (concat $ map showType locals)
        ++ indent 1 ++ "operand stack\n"
        ++ indent 3 ++ "[" ++ (concat $ map showMaybeValtype operandStack) ++ " ]" ++ "\n"
        ++ indent 1 ++ "control stack\n" ++ (concat $ map showControlFrame $ zip [0..] controlStack)
        -- ++ "\n"

instance Show ControlFrame where
    show (ControlFrame labelTypes resultType height unreachable) =
        "label types [" ++ (concat $ map show labelTypes) ++ " ]\n"
            ++ indent 5 ++ "result type" ++ (showResultType resultType) ++ "\n"
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


showMaybeValtype :: Maybe ValType -> String
showMaybeValtype maybeValtype =
    case maybeValtype of
        Just valtype -> show valtype
        Nothing -> " Unknown"


showResultType :: ResultType -> String
showResultType resulttype =
    case resulttype of
        Just result -> show result
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
                ++ indent 3 ++ "[" ++ (concat $ map showMaybeValtype operandStack) ++ " ]"
                ++ "\n"


-- PRINT


printStep :: Component ParserIdX -> ContextState ()
printStep component = do
    context <- get
    liftIO $ putStrLn $ show context
    liftIO $ putStrLn "🞅∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙🞅"
    liftIO $ putStrLn "∙component∙"
    liftIO $ printTree component
    liftIO $ putStr "\n"
    return ()


printFuncStep :: Component ParserIdX -> ContextState ()
printFuncStep component = do
    liftIO $ putStrLn "λ••••••••••••••••"
    liftIO $ putStrLn $ "checking:"
    liftIO $ printTree component
    liftIO $ putStr "\n"
    return ()


printBlockEntry :: ValidationState ()
printBlockEntry = do
    (_, localContext) <- get
    liftIO $ putStrLn "🞎∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙"
    liftIO $ putStrLn $ show localContext ++ "\n"
    return ()


printBlockExit :: ValidationState ()
printBlockExit = do
    (_, localContext) <- get
    liftIO $ putStrLn "∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙🞎"
    liftIO $ putStrLn $ show localContext ++ "\n"
    return ()


printInstructionStep :: Instruction ParserIdX -> ValidationState ()
printInstructionStep instruction = do
    (_, localContext) <- get
    liftIO $ putStrLn "⬚∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙"
    liftIO $ printTree instruction
    liftIO $ putStrLn $ "\n" ++ showInstructionContext localContext ++ "\n"
    return ()



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
                ++ " already defined in " ++ indexSpaceName ++ " index space"
        Underflow ->
            "Pop operation undeflows the current block"
        PopEmptyOps ->
            "Cannot pop an empty operand stack"
        PopEmptyFrames ->
            "Attempt to pop a control frame but there are none"
        OperandMismatch ->
            "Actual operand does not match expected operand"
        ExitHeightMismatch ->
            "The operand stack was not returned to its initial height at the end of this func"


printImportOrderError :: IO ()
printImportOrderError = putStrLn $ "🗙 Imports must come before any funcs, tables, memories, or globals in a valid module"
