module Check where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe (listToMaybe)
import Data.Word (Word32)
import qualified Text.Parsec as Parsec

import Parser

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
    -- , funcs :: [(MaybeIdent, FuncType)]
    , funcs :: [(MaybeIdent, TypeIndex)]
    -- add tables and mems
    , globals :: [(MaybeIdent, GlobalType)]
    , start :: Maybe ParserIdX
    , valid :: Bool
    }


data LocalContext = LocalContext
    { locals :: [(MaybeIdent, ValType)]
    , labels :: [(MaybeIdent, ResultType)]
    , return_ :: (MaybeIdent, ResultType)
    }


type Types = [(MaybeIdent, FuncType)]

type TypeIndex = Word32

type Funcs = [(MaybeIdent, TypeIndex)]



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
                        if (valid context) then do
                            putStrLn $ show context
                            putStrLn "④ Checking func bodies, starts and exports..."
                            checkedContext <- execStateT (check_ components) context
                            if (valid checkedContext) then do
                                putStrLn "✓ Module is valid"
                            else
                                putStrLn "🗙 Invalid module"
                        else
                            putStrLn "🗙 Invalid module"
                        return ()
                    else do
                        putStrLn importOrderError
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
        Import _ _ _ -> (True && stillImporting, stillImporting)
        Func _ _ _ _ -> (inOrder, False && stillImporting)
        Global _ _ _ -> (inOrder, False && stillImporting)
        -- add tables and mems
        _            -> (inOrder, stillImporting)



-- CONTEXT PREPASS

{-| Build the global context

  The first pass collects types.
  The second pass collects imports, funcs, globals, and exports. Any InlineTypes that
  do not exist by some other name in the context are added in this pass.
-}
  

makeContext :: Components ParserIdX -> StateT Context IO ()
makeContext components = do
    liftIO $ putStrLn "② Adding types to the context..."
    mapM_ registerType components
    context <- get
    liftIO $ putStrLn "③ Adding imports, funcs, and globals to the context..."
    mapM_ registerComponent components


registerType :: Component ParserIdX -> StateT Context IO ()
registerType component = do
     context <- get
     case component of
         Type maybeId functype -> do
             liftIO $ printStep context component
             updatedTypes <- addContextEntry "type" maybeId functype (types context)
             put (context { types = updatedTypes })
         _ -> return ()


registerComponent :: Component ParserIdX -> StateT Context IO ()
registerComponent component = do
    context <- get
    case component of
        Type maybeId functype ->
            return ()
        Import _ _ importdesc -> do
            liftIO $ printStep context component
            registerImport importdesc
        Func maybeId typeuse _ _ -> do
            liftIO $ printStep context component
            registerFunc maybeId typeuse
        Start _ ->
            return ()
        Global maybeId globaltype _ -> do
            updatedGlobals <- addContextEntry "global" maybeId globaltype (globals context)
            put (context { globals = updatedGlobals })
        Export _ _ ->
            return ()


registerImport :: ImportDescription ParserIdX -> StateT Context IO ()
registerImport importdesc = do
    context <- get
    case importdesc of
        FuncImport maybeId typeuse ->
            registerFunc maybeId typeuse
        -- add table, memory, and global imports here


registerFunc :: MaybeIdent -> TypeUse ParserIdX -> StateT Context IO ()
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
                    put (context { types = updatedTypes })
                    context <- get
                    maybeIndex <- return $ lookupType (FuncType params results) (types context)
                    case maybeIndex of
                        Just typeIndex -> do
                            updatedFuncs <- addContextEntry "func" maybeId typeIndex (funcs context)
                            put (context { funcs = updatedFuncs })
                        Nothing ->
                            liftIO $ print "I just added that type. This must be a compiler bug."



{-| Add a context entry

   Each context entry is a tuple with maybe and ident and a type definition or reference (a typedref).
   The position of each entry is its index.
-}

addContextEntry :: String -> MaybeIdent -> a -> [(MaybeIdent, a)] -> StateT Context IO [(MaybeIdent, a)]
addContextEntry indexSpaceName maybeId typedref indexSpace =
    case maybeId of
        Just id ->
            if uniqueIdentifier id indexSpace then
                return (indexSpace ++ [(maybeId, typedref)])
            else do
                liftIO $ putStrLn $ "Id " ++ show id
                    ++ " already defined in " ++ indexSpaceName ++ " index space."
                return indexSpace
        Nothing -> return (indexSpace ++ [(Nothing, typedref)])


uniqueIdentifier :: Ident -> [(MaybeIdent,a)] -> Bool
uniqueIdentifier id indexSpace =
    null [ j | (Just j, _) <- indexSpace, j == id ]



-- TYPEUSE

  
checkTypeUse :: ParserIdX -> StateT Context IO (Maybe TypeIndex)
checkTypeUse idx = do
    context <- get
    case lookupTypeRef idx (types context) of
        Just _ ->
            case idx of
                Left n   -> return $ Just n
                Right id -> return $ resolveId (Just id) (types context)
        Nothing -> do
            liftIO $ putStrLn $ missingTypeError idx
            put (context { valid = False })
            return Nothing 

  
checkTypeUseWithDeclarations :: ParserIdX -> Params -> Results -> StateT Context IO (Maybe TypeIndex)
checkTypeUseWithDeclarations idx params results = do
    context <- get
    case lookupTypeRef idx (types context) of
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
                        liftIO $ putStrLn $ typeMismatchError idx
                        put (context { valid = False })
                        return Nothing
        Nothing -> do
            liftIO $ putStrLn $ missingTypeError idx
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
 
check_ :: Components ParserIdX -> StateT Context IO ()
check_ components =
    mapM_ checkFuncs components


checkFuncs :: Component ParserIdX -> StateT Context IO ()
checkFuncs component = do
    context <- get
    case component of
        Type _ _ ->
            return ()
        Import _ _ _ ->
            return ()
        Func maybeId typeuse _ _ -> do
            -- check func body
            liftIO $ printStep context component
            return ()
        Start idx -> do
            liftIO $ printStep context component
            checkStart idx
        Global maybeId globaltype _ ->
            -- check function expressions
            return ()
        Export _ exportdesc -> do
            liftIO $ printStep context component
            checkExport exportdesc


checkStart :: ParserIdX -> StateT Context IO ()
checkStart idx = do
    context <- get
    if null (start context) then
        case lookupFuncRef idx (funcs context) of
            Just _ -> do
                put (context { start = Just idx })
                return ()
            Nothing -> do
                put (context { valid = False })
                liftIO $ putStrLn $ missingTypeError idx
                return ()
    else do
        put (context { valid = False })
        liftIO $ putStrLn $ multipleStartError
        return ()
   

checkExport :: ExportDescription ParserIdX -> StateT Context IO ()
checkExport exportdesc = do
    context <- get
    case exportdesc of
       FuncExport idx ->
           case lookupFuncRef idx (funcs context) of
               Just _ ->
                   return ()
               Nothing -> do
                   put (context { valid = False })
                   liftIO $ putStrLn $ missingTypeError idx
                   return ()

 


-- LOOKUP


lookupTypeRef :: ParserIdX -> Types -> Maybe FuncType
lookupTypeRef idx types =
    case idx of
        Left n   -> lookupByIndex n types
        Right id -> lookupById id types


lookupFuncRef :: ParserIdX -> Funcs -> Maybe TypeIndex
lookupFuncRef idx funcs =
    case idx of
        Left n   -> lookupByIndex n funcs
        Right id -> lookupById id funcs



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


-- SHOW


instance Show Context where
    show (Context types funcs globals start valid) = "• context •" ++ "\n"
        ++ indent 1 ++ "types\n" ++ (concat $ map showType types)
        ++ indent 1 ++ "funcs\n" ++ (concat $ map showTypeRef funcs)
        ++ indent 1 ++ "globals\n" ++ (concat $ map showType globals)
        ++ indent 1 ++ "start" ++ showMaybeIdX start ++ "\n"
        ++ indent 1 ++ "valid " ++ show valid
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


printStep :: Context -> Component ParserIdX -> IO ()
printStep context component = do
    putStrLn $ show context
    putStrLn "+----------------"
    putStrLn "• component •"
    printTree component
    putStrLn ""



-- ERRORS


missingTypeError :: ParserIdX -> String
missingTypeError idx =
    "🗙 I tried looking for a type with the identifier" ++ (showIdX idx) ++ " but I could not find one.\n"

missingFuncError :: ParserIdX -> String
missingFuncError idx =
    "🗙 I tried looking for a func with the identifier" ++ (showIdX idx) ++ " but I could not find one.\n"

typeMismatchError :: ParserIdX -> String
typeMismatchError idx =
    "🗙 The inline declarations used in this type use do not match the reference type" ++ (showIdX idx) ++ ".\n"


multipleStartError :: String
multipleStartError =
    "🗙 A start component has already registered and entry point to this module."

importOrderError :: String
importOrderError =
    "🗙 Imports must come before any funcs, tables, memories, or globals in a valid module."
