{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Graphics.Implicit.ExtOpenScad.Eval.Statement where

import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Util.OVal
import Graphics.Implicit.ExtOpenScad.Util.ArgParser
import Graphics.Implicit.ExtOpenScad.Util.StateC
import Graphics.Implicit.ExtOpenScad.Eval.Expr
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)

import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import           Control.Monad.State (get, liftIO)
import qualified System.FilePath as FilePath

runStatementI :: StatementI -> StateC ()

runStatementI (StatementI lineN (pat := expr)) = do
    val <- evalExpr expr
    let posMatch = matchPat pat val
    case (getErrors val, posMatch) of
        (Just err,  _ ) -> errorC lineN err
        (_, Just match) -> modifyVarLookup $ Map.union match
        (_,   Nothing ) -> errorC lineN "pattern match failed in assignment"

runStatementI (StatementI lineN (Echo exprs)) = do
    let
        show2 (OString s) = s
        show2 x = show x
    vals <- mapM evalExpr exprs
    case getErrors (OList vals) of
        Nothing  -> liftIO $ putStrLn $ concat $ map show2 vals
        Just err -> errorC lineN err

runStatementI (StatementI lineN (For pat expr loopContent)) = do
    val <- evalExpr expr
    case (getErrors val, val) of
        (Just err, _)      -> errorC lineN err
        (_, OList vals) -> Monad.forM_ vals $ \v ->
            case matchPat pat v of
                Just match -> do
                    modifyVarLookup $ Map.union match
                    runSuite loopContent
                Nothing -> return ()
        _ -> return ()

runStatementI (StatementI lineN (If expr a b)) = do
    val <- evalExpr expr
    case (getErrors val, val) of
        (Just err,  _  )  -> errorC lineN ("In conditional expression of if statement: " ++ err)
        (_, OBool True )  -> runSuite a
        (_, OBool False)  -> runSuite b
        _                 -> return ()

runStatementI (StatementI lineN (NewModule name argTemplate suite)) = do
    opts <- languageOptions
    argTemplate' <- Monad.forM argTemplate $ \(name, defexpr) -> do
        defval <- mapMaybeM evalExpr defexpr
        return (name, defval)
    (varlookup, _, path, _, _) <- get
    runStatementI $ StatementI lineN $ (Name name :=) $ LitE $ OModule $ \vals -> do
        newNameVals <- Monad.forM argTemplate' $ \(name, maybeDef) -> do
            val <- case maybeDef of
                Just def -> argument name `defaultTo` def
                Nothing  -> argument name
            return (name, val)
        let
{-
            children = ONum $ fromIntegral $ length vals
            child = OModule $ \vals -> do
                n :: ℕ <- argument "n";
                return $ return $ return $
                    if n <= length vals
                        then vals !! n
                        else OUndefined
            childBox = OFunc $ \n -> case fromOObj n :: Maybe ℕ of
                Just n  | n < length vals -> case vals !! n of
                    -- _ -> toOObj $ getBox3 obj3
                    -- _ -> toOObj $ getBox2 obj2
                    _ -> OUndefined
                _ -> OUndefined
            newNameVals' = newNameVals ++ [("children", children),("child", child), ("childBox", childBox)]
-}
            varlookup' = Map.union (Map.fromList newNameVals) varlookup
            suiteVals  = runSuiteCapture varlookup' path opts suite
        return suiteVals

runStatementI (StatementI sourcePos (ModuleCall name argsExpr suite)) = do
        opts <- languageOptions
        maybeMod  <- lookupVar name
        (varlookup, _, path, _, _) <- get
        argsVal   <- Monad.forM argsExpr $ \(posName, expr) -> do
            val <- evalExpr expr
            return (posName, val)
        newVals <- case maybeMod of
            Just (OModule modul) -> do
                childVals <- fmap reverse $ liftIO $ runSuiteCapture varlookup path opts suite
                let
                    argparser = modul childVals
                    ioNewVals = case fst $ argMap argsVal argparser of
                        Just iovals -> iovals
                        Nothing     -> return []
                liftIO ioNewVals
            Just (OVargsModule name modul) -> do
                _ <- modul name sourcePos argsVal suite runSuite -- no values are returned
                return []
            Just foo -> do
                    case getErrors foo of
                        Just err -> errorC sourcePos err
                        Nothing  -> errorC sourcePos "Object called not module!"
                    return []
            Nothing -> do
                errorC sourcePos $ "Module " ++ name ++ " not in scope."
                return []
        pushVals newVals

runStatementI (StatementI _ (Include name injectVals)) = do
    name' <- getRelPath name
    content <- liftIO $ readFile name'
    case parseProgram name content of
        Left e -> liftIO $ putStrLn $ "Error parsing " ++ name ++ ":" ++ show e
        Right sts -> withPathShiftedBy (FilePath.takeDirectory name) $ do
            vals <- getVals
            putVals []
            runSuite sts
            vals' <- getVals
            if injectVals then putVals (vals' ++ vals) else putVals vals

runStatementI (StatementI _ (Sequence suite)) =
    runSuite suite

runStatementI (StatementI _ NewFunction{}) =
    return ()

runStatementI (StatementI _ DoNothing) =
    return ()

runSuite :: [StatementI] -> StateC ()
runSuite = Monad.mapM_ runStatementI

runSuiteCapture :: VarLookup -> FilePath -> LanguageOpts -> [StatementI] -> IO [OVal]
runSuiteCapture varlookup path opts suite = do
    (res, _) <- State.runStateT
        (runSuite suite >> getVals)
        (varlookup, [], path, opts, [] )
    return res

runSuiteInModule :: FilePath -> LanguageOpts -> [StatementI] -> VarLookup -> IO [OVal]
runSuiteInModule path opts suite varlookup = runSuiteCapture varlookup path opts suite
