module Check where

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Text.Parsec as Parsec

import Parser



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
                    return ()
                  where emptyContext = Context { types = [], funcs = [], globals = [] }


makeContext :: Components ParserIdX -> StateT Context IO ()
makeContext components = do
    mapM_ addComponent components


addComponent :: Component ParserIdX -> StateT Context IO ()
addComponent component = do
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


-- SHOW


printStep :: Context -> Component ParserIdX -> IO ()
printStep context component = do
    putStr $ show context
    putStrLn "-----"
    printTree component
    putStrLn ""


instance Show Context where
    show (Context types funcs globals) = "context" ++ "\n"
        ++ indent 1 ++ "types\n" ++ (concat $ map showType types)
        ++ indent 1 ++ "funcs\n" ++ (concat $ map showType funcs)
        ++ indent 1 ++ "globals\n" ++ (concat $ map showType globals)


showType :: Show a => (MaybeIdent, a) -> String
showType (maybeId, typedef) =
    indent 3
        ++ case maybeId of
               Just id -> show id ++ " :" ++ show typedef ++ "\n"
               Nothing -> "_ :" ++ show typedef ++ "\n"


instance Show FuncType where
    show (FuncType params results) =
        concat $ (map (\p -> p ++ " ->") $ showParams params) ++ showResults results

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
