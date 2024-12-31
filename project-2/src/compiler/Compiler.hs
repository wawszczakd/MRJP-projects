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
            (_, envWithFuncs) = runState (insertFuncs topDefs) (1, 1, (Data.Map.empty, Data.Map.empty), Data.Map.empty)
            (programBody, _) = runState (compileTopDefs topDefs) envWithFuncs
            programHead = [ LLVMFunDec LLVMInt "readInt" []
                          , LLVMFunDec LLVMStr "readString" []
                          , LLVMFunDec LLVMVoid "printInt" [LLVMArgDec LLVMInt]
                          , LLVMFunDec LLVMVoid "printString" [LLVMArgDec LLVMStr] ]
        in
            programHead ++ programBody
    
    insertFuncs :: [TopDef] -> CompilerMonad ()
    insertFuncs topDefs = do
        let builtInFuncs = [ ("readInt", LLVMInt)
                           , ("readString", LLVMStr)
                           , ("printInt", LLVMVoid)
                           , ("printString", LLVMVoid) ]
        modify (\(nextLoc, nextReg, (funEnv, varEnv), store) ->
            (nextLoc, nextReg, (Data.List.foldr (\(name, entry) -> Data.Map.insert (Ident name) entry) funEnv builtInFuncs, varEnv), store))
        foldM_ (\_ topDef -> do
                case topDef of
                    TopFunDef _ (FnDef _ typ name _ _) -> do
                        let retType = typeToLLVM typ
                        modify (\(nextLoc, nextReg, (funEnv, varEnv), store) ->
                            (nextLoc, nextReg, (Data.Map.insert name retType funEnv, varEnv), store))
                    _ -> return ()
                return ()) () topDefs
    
    compileTopDefs :: [TopDef] -> CompilerMonad [LLVMInstr]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [LLVMEmpty] ++ topDefCode) [] topDefs
    
    compileTopDef :: TopDef -> CompilerMonad [LLVMInstr]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args block)) = do
        (nextLoc, nextReg, env, store) <- get
        
        mapM_ insertArg args
        funBody <- compileBlock block
        let
            retType = typeToLLVM typ
            llvmArgs = zipWith (formatArg nextReg) [0..] args
            funDef = LLVMFunDef retType name llvmArgs (funBody ++ [addReturn typ])
        
        put (nextLoc, nextReg, env, store)
        return $ [funDef]
        where
            insertArg :: Arg -> CompilerMonad ()
            insertArg (Ar _ _ name) = do
                (nextLoc, nextReg, (funEnv, varEnv), store) <- get
                let
                    newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = Data.Map.insert nextLoc (RegVal (LLVMReg nextReg)) store
                put (nextLoc + 1, nextReg + 1, (funEnv, newVarEnv), newStore)
            
            formatArg :: Integer -> Integer -> Arg -> LLVMArg
            formatArg curNextReg index (Ar _ typ (Ident name)) =
                LLVMArg (typeToLLVM typ) (LLVMReg (curNextReg + index))
            
            addReturn :: Type -> LLVMInstr
            addReturn (Void _) = LLVMRetVoid
            addReturn (Int _) = LLVMRet (IntVal 0)
            addReturn (Bool _) = LLVMRet (BoolVal False)
    
    compileBlock :: Block -> CompilerMonad [LLVMInstr]
    compileBlock (Blck _ stmts) = do
        (_, _, env, _) <- get
        instrs <- compileStmts stmts
        (nextLoc, nextReg, _, store) <- get
        put (nextLoc, nextReg, env, store)
        return instrs
