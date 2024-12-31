module StmtCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    import ExprCompiler
    import LLVMInstructions
    import UtilsCompiler
    
    compileStmts :: [Stmt] -> CompilerMonad [LLVMInstr]
    compileStmts stmts = do
        stmtResults <- mapM compileStmt stmts
        return $ concat stmtResults
    
    compileStmt :: Stmt -> CompilerMonad [LLVMInstr]
    
    compileStmt (Empty _) =
        return []
    
    compileStmt (BStmt _ (Blck _ stmts)) = do
        (_, _, env, _) <- get
        instrs <- compileStmts stmts
        (nextLoc, nextReg, _, store) <- get
        put (nextLoc, nextReg, env, store)
        return instrs
    
    compileStmt (Decl _ typ vars) = do
        instrs <- concat <$> mapM (insertVar typ) vars
        return instrs
        where
            insertVar :: Type -> Item -> CompilerMonad [LLVMInstr]
            insertVar typ (NoInit _ name) = do
                (nextLoc, nextReg, (funEnv, varEnv), store) <- get
                let newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = case typ of
                        (Int _)  -> Data.Map.insert nextLoc (IntVal 0) store
                        (Bool _) -> Data.Map.insert nextLoc (BoolVal False) store
                        (Str _)  -> Data.Map.insert nextLoc (StrVal "") store
                put (nextLoc + 1, nextReg, (funEnv, newVarEnv), newStore)
                return []
            insertVar typ (Init _ name expr) = do
                (val, instrs) <- compileExpr expr
                (nextLoc, nextReg, (funEnv, varEnv), store) <- get
                let newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = Data.Map.insert nextLoc val store
                put (nextLoc + 1, nextReg, (funEnv, newVarEnv), newStore)
                return (instrs)
    
    compileStmt (Ass _ (LVar _ name) expr) = do
        (val, instrs) <- compileExpr expr
        (nextLoc, nextReg, (funEnv, varEnv), store) <- get
        let Just loc = Data.Map.lookup name varEnv
            newStore = Data.Map.insert loc val store
        put (nextLoc, nextReg, (funEnv, varEnv), newStore)
        return instrs
    
    compileStmt (Incr _ (LVar _ (Ident name))) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (Decr _ (LVar _ (Ident name))) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (Ret _ expr) = do
        (val, instrs) <- compileExpr expr
        return $ instrs ++ [LLVMRet val]
    
    compileStmt (VRet _) =
        return [LLVMRetVoid]
    
    compileStmt (Cond _ expr stmt) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (CondElse _ expr stmt1 stmt2) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (While _ expr stmt) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (SExp _ expr) = do
        (_, instrs) <- compileExpr expr
        return instrs
