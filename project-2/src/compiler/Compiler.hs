module Compiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import LLVMInstructions
    import StmtCompiler
    import UtilsCompiler
    
    compileProgram :: Program -> [LLVMInstr]
    compileProgram (Prog _ topDefs) =
        let
            (_, envWithFuncs) = runState (insertFuncs topDefs) (0, 0, 1, (Data.Map.empty, Data.Map.empty), Data.Map.empty)
            (programBody, _) = runState (compileTopDefs topDefs) envWithFuncs
            programHead = [ LLVMFunDec LLVMVoid "printInt" [LLVMArgDec LLVMInt]
                          , LLVMFunDec LLVMVoid "printString" [LLVMArgDec LLVMStr]
                          , LLVMFunDec LLVMVoid "error" []
                          , LLVMFunDec LLVMInt "readInt" []
                          , LLVMFunDec LLVMStr "readString" [] ]
        in
            programHead ++ programBody
    
    insertFuncs :: [TopDef] -> CompilerMonad ()
    insertFuncs topDefs = do
        let builtInFuncs = [ ("printInt", LLVMVoid)
                           , ("printString", LLVMVoid)
                           , ("error", LLVMVoid)
                           , ("readInt", LLVMInt)
                           , ("readString", LLVMStr) ]
        modify (\(nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) ->
            (nextLoc, nextReg, nextLabel, (Data.List.foldr (\(name, entry) -> Data.Map.insert (Ident name) entry) funEnv builtInFuncs, varEnv), store))
        foldM_ (\_ topDef -> do
                case topDef of
                    TopFunDef _ (FnDef _ typ name _ _) -> do
                        let retType = typeToLLVM typ
                        modify (\(nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) ->
                            (nextLoc, nextReg, nextLabel, (Data.Map.insert name retType funEnv, varEnv), store))
                    _ -> return ()
                return ()) () topDefs
    
    compileTopDefs :: [TopDef] -> CompilerMonad [LLVMInstr]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [LLVMEmpty] ++ topDefCode) [] topDefs
    
    compileTopDef :: TopDef -> CompilerMonad [LLVMInstr]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args (Blck _ stmts))) = do
        (nextLoc, nextReg, nextLabel, env, store) <- get
        
        mapM_ insertArg args
        instrs <- compileStmts stmts
        let
            funBody = LLVMLabel (LLVMLab 0) : instrs
            retType = typeToLLVM typ
            llvmArgs = zipWith (formatArg nextReg) [0..] args
            lastInstr = if Data.List.null funBody then Nothing else Just (last funBody)
            funDef = case lastInstr of
                        Just (LLVMRet _) -> LLVMFunDef retType name llvmArgs funBody
                        Just LLVMRetVoid -> LLVMFunDef retType name llvmArgs funBody
                        _ -> LLVMFunDef retType name llvmArgs (funBody ++ [addReturn typ])
        
        put (nextLoc, nextReg, nextLabel, env, store)
        return [funDef]
        where
            insertArg :: Arg -> CompilerMonad ()
            insertArg (Ar _ typ name) = do
                (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
                let
                    newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = Data.Map.insert nextLoc (RegVal (typeToLLVM typ) (LLVMReg nextReg)) store
                put (nextLoc + 1, nextReg + 1, nextLabel, (funEnv, newVarEnv), newStore)
            
            formatArg :: Integer -> Integer -> Arg -> LLVMArg
            formatArg curNextReg index (Ar _ typ (Ident name)) =
                LLVMArg (typeToLLVM typ) (LLVMReg (curNextReg + index))
            
            addReturn :: Type -> LLVMInstr
            addReturn (Void _) = LLVMRetVoid
            addReturn (Int _) = LLVMRet (toLLVMValT (IntVal 0))
            addReturn (Bool _) = LLVMRet (toLLVMValT (BoolVal False))

