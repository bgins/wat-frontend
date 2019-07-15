module Check where

import Control.Monad.IO.Class
import Control.Monad.State
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
    , funcs :: [(MaybeIdent, FuncType)]
    -- add tables and mems
    , globals :: [(MaybeIdent, GlobalType)]
    }


data LocalContext = LocalContext
    { locals :: [(MaybeIdent, ValType)]
    , labels :: [(MaybeIdent, ResultType)]
    , return_ :: (MaybeIdent, ResultType)
    }


type Types = [(MaybeIdent, FuncType)]



-- CHECK


check :: String -> IO ()
check filepath = do
    text <- readFile filepath
    case Parsec.parse parse filepath text of
        Left err  -> putStrLn $ show err
        Right ast -> do
            case ast of
                Module maybeId components -> do
                    context <- execStateT (makeContext components) emptyContext
                    putStr $ show context
                    checks <- checkModule context components
                    if and checks then
                      putStrLn "Module is valid."
                    else
                      putStrLn "Invalid module."
                    return ()
                  where emptyContext = Context { types = [], funcs = [], globals = [] }



-- CONTEXT PREPASS

  
makeContext :: Components ParserIdX -> StateT Context IO ()
makeContext components = do
    mapM_ registerComponent components

  
registerComponent :: Component ParserIdX -> StateT Context IO ()
registerComponent component = do
    context <- get
    liftIO $ printStep context component
    case component of
        Type maybeId functype -> do
            updatedTypes <- addId "type" maybeId functype (types context)
            put (context { types = updatedTypes })
        Import _ _ importdesc ->
            if null (funcs context) && null (globals context) then
                case importdesc of
                    FuncImport maybeId typeuse ->
                        case typeuse of
                            InlineType params results -> do
                                updatedFuncs <- addId "func" maybeId (FuncType params results) (funcs context)
                                put (context { funcs = updatedFuncs })
                            _ -> return () -- validate these cases in next pass
            else
                fail "Imports must come before any funcs, tables, memories, or globals in a valid module."
        Func maybeId typeuse _ _ ->
            case typeuse of
                InlineType params results -> do
                    updatedFuncs <- addId "func" maybeId (FuncType params results) (funcs context)
                    put (context { funcs = updatedFuncs })
                _ -> return () -- validate these cases in next pass
        Start _ -> return ()
        Global maybeId globaltype _ -> do
            updatedGlobals <- addId "func" maybeId globaltype (globals context)
            put (context { globals = updatedGlobals })
        Export _ _ -> return ()


addId :: String -> MaybeIdent -> a -> [(MaybeIdent, a)] -> StateT Context IO [(MaybeIdent, a)]
addId indexSpaceName maybeId typedef indexSpace = do
    case maybeId of
        Just id ->
            if uniqueIdentifier id indexSpace then
                return (indexSpace ++ [(maybeId, typedef)])
            else do
                liftIO $ putStrLn $ "Id " ++ show id
                    ++ " already defined in " ++ indexSpaceName ++ " index space."
                return indexSpace
        Nothing -> return (indexSpace ++ [(Nothing, typedef)])


uniqueIdentifier :: Ident -> [(MaybeIdent,a)] -> Bool
uniqueIdentifier id indexSpace =
    null [ j | (Just j, _) <- indexSpace, j == id ]



-- CHECK


checkModule :: Context -> Components ParserIdX -> IO [Bool]
checkModule context components =
    mapM (checkComponent context) components


checkComponent :: Context -> Component ParserIdX -> IO Bool
checkComponent context component = do
    case component of
        Type _ _ -> return True
        Import moduleName name importdesc -> do
          checkImport context importdesc
        Func maybeId typeuse locals instructions -> return True
        -- check Start after all Funcs have been added to see if idx is a valid function
        Start _ -> return True
        Global maybeId globaltype instructions -> return True
        Export name exportdesc -> return True



-- IMPORTS


checkImport :: Context -> ImportDescription ParserIdX -> IO Bool
checkImport context importdesc = do
    case importdesc of
        FuncImport maybeId typeuse -> do
            case typeuse of
                TypeUse idx ->
                    checkTypeUse idx context
                TypeUseWithDeclarations idx params results ->
                    checkTypeUseWithDeclarations idx params results context
                InlineType params results ->
                    return True  -- type defined locally, should be valid



-- TYPEUSE

  
checkTypeUse :: ParserIdX -> Context -> IO Bool
checkTypeUse idx context =
    case lookupType idx (types context) of
        Just _ -> return True
        Nothing -> do
          putStrLn $ showMissingType idx
          return False 

  
checkTypeUseWithDeclarations :: ParserIdX -> Params -> Results -> Context -> IO Bool
checkTypeUseWithDeclarations idx params results context =
    case lookupType idx (types context) of
        Just functype -> do
            case functype of
                FuncType contextParams contextResults -> do
                    paramChecks <- checkParams contextParams params
                    resultChecks <- checkResults contextResults results
                    if (and paramChecks) && (and resultChecks) then
                        return True
                    else do
                        putStrLn $ showTypeMismatch idx
                        return False
        Nothing -> do
            putStrLn $ showMissingType idx
            return False 
 


-- PARAMS AND RESULTS
 

checkParams :: Params -> Params -> IO [Bool]
checkParams contextParams referenceParams = do
    mapM checkParam $ zip contextParams referenceParams


checkParam :: (Param, Param) -> IO Bool
checkParam (contextParam, referenceParam) = do
    case referenceParam of
        Param _ referenceValtype ->
            case contextParam of
                Param _ contextValtype ->
                    if (referenceValtype == contextValtype) then
                        return True
                    else
                        return False
  
checkResults :: Results -> Results -> IO [Bool]
checkResults contextResults referenceResults = do
    mapM checkResult $ zip contextResults referenceResults


checkResult :: (Result, Result) -> IO Bool
checkResult (contextResult, referenceResult) = do
    case referenceResult of
        Result referenceValtype ->
            case contextResult of
                Result contextValtype ->
                    if (referenceValtype == contextValtype) then
                        return True
                    else
                        return False
 

-- checkParamsResults :: a -> a -> IO [Bool]
-- checkParamsResults types references = do
--     mapM checkParamResult $ zip types references


-- checkParamResult :: (a, a) -> IO Bool
-- checkParamResult (type_, reference) = do
--     case reference of
--         Param _ referenceValtype ->
--             case type_ of
--                 Param _ valtype ->
--                     if (referenceValtype == valtype) then
--                         return True
--                     else
--                         return False
--         Result referenceValtype ->
--             case type_ of
--                 Result valtype ->
--                     if (referenceValtype == valtype) then
--                         return True
--                     else
--                         return False
 


-- LOOKUP

  
lookupType :: ParserIdX -> Types -> Maybe FuncType
lookupType idx types =
    case idx of
        Left n   -> lookupByIndex n types
        Right id -> lookupById id types


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
           

-- SHOW


instance Show Context where
    show (Context types funcs globals) = "• context •" ++ "\n"
        ++ indent 1 ++ "types\n" ++ (concat $ map showType types)
        ++ indent 1 ++ "funcs\n" ++ (concat $ map showType funcs)
        ++ indent 1 ++ "globals\n" ++ (concat $ map showType globals)
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


printStep :: Context -> Component ParserIdX -> IO ()
printStep context component = do
    putStr $ show context
    putStrLn "-----"
    printTree component
    putStrLn ""



-- SHOW ERRORS


showMissingType :: ParserIdX -> String
showMissingType idx =
    "An import failed during typechecking. The type" ++ (showIdX idx) ++ " does not exist."


showTypeMismatch :: ParserIdX -> String
showTypeMismatch idx =
    "An import failed during typechecking. The inline declarations do not match the reference type" ++ (showIdX idx)
