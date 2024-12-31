module StmtCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    import ExprCompiler
    import UtilsCompiler
    
    compileStmts :: [Stmt] -> CompilerMonad [String]
    compileStmts stmts = do
        stmtResults <- mapM compileStmt stmts
        return $ concat stmtResults
    
    compileStmt :: Stmt -> CompilerMonad [String]
    
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
            insertVar :: Type -> Item -> CompilerMonad [String]
            insertVar typ (NoInit _ name) = do
                (nextLoc, nextReg, (funEnv, varEnv), store) <- get
                let newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = Data.Map.insert nextLoc nextReg store
                put (nextLoc + 1, nextReg + 1, (funEnv, newVarEnv), newStore)
                let initInstr = case typ of
                        (Int _)  -> "    %" ++ show nextReg ++ " = add i32 0, 0"
                        (Bool _) -> "    %" ++ show nextReg ++ " = add i1 0, 0"
                        (Str _)  -> "    %" ++ show nextReg ++ " = bitcast i8* null to i8*"
                return [initInstr]
            insertVar typ (Init _ name expr) = do
                (exprVal, instrs) <- compileExpr expr
                (nextLoc, nextReg, (funEnv, varEnv), store) <- get
                case exprVal of
                    In val -> do
                        let initInstr = "    %" ++ show nextReg ++ " = add i32 " ++ show val ++ ", 0"
                        let newVarEnv = Data.Map.insert name nextLoc varEnv
                            newStore = Data.Map.insert nextLoc nextReg store
                        put (nextLoc + 1, nextReg + 1, (funEnv, newVarEnv), newStore)
                        return (instrs ++ [initInstr])
                    Bo val -> do
                        let initInstr = "    %" ++ show nextReg ++ " = add i1 " ++ (if val then "1" else "0") ++ ", 0"
                        let newVarEnv = Data.Map.insert name nextLoc varEnv
                            newStore = Data.Map.insert nextLoc nextReg store
                        put (nextLoc + 1, nextReg + 1, (funEnv, newVarEnv), newStore)
                        return (instrs ++ [initInstr])
                    Re reg -> do
                        let newVarEnv = Data.Map.insert name nextLoc varEnv
                            newStore = Data.Map.insert nextLoc reg store
                        put (nextLoc + 1, nextReg, (funEnv, newVarEnv), newStore)
                        return (instrs)
    
    compileStmt (Ass _ (LVar _ (Ident name)) expr) =
        return ["    ; Ass stmt"]
    
    compileStmt (Incr _ (LVar _ (Ident name))) =
        return ["    ; Incr stmt"]
    
    compileStmt (Decr _ (LVar _ (Ident name))) =
        return ["    ; Decr stmt"]
    
    compileStmt (Ret _ expr) = do
        (val, instrs) <- compileExpr expr
        let retInstr = case val of
                In intVal -> "    ret i32 " ++ show intVal
                Bo boolVal -> "    ret i1 " ++ if boolVal then "1" else "0"
                Re reg -> "    ret i32 %" ++ show reg
                _ -> error "Unsupported return type"
        return $ instrs ++ [retInstr]
    
    compileStmt (VRet _) =
        return ["    ret void"]
    
    compileStmt (Cond _ expr stmt) =
        return ["    ; Cond stmt"]
    
    compileStmt (CondElse _ expr stmt1 stmt2) =
        return ["    ; CondElse stmt"]
    
    compileStmt (While _ expr stmt) =
        return ["    ; While stmt"]
    
    compileStmt (SExp _ expr) = do
        (_, instrs) <- compileExpr expr
        return instrs
