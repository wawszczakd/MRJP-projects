module StmtCompiler where
    import AbsLatte
    import Control.Monad.State
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
        env <- get
        instrs <- compileStmts stmts
        put env
        return instrs
    
    compileStmt (Decl _ typ vars) =
        -- env <- ask
        -- env' <- foldM insertVar env vars
        -- where
        --     insertVar :: Env -> Item -> CompilerMonad Env
        return ["    ; Decl stmt"]
    
    compileStmt (Ass _ (LVar _ (Ident name)) expr) =
        return ["    ; Ass stmt"]
    
    compileStmt (Incr _ (LVar _ (Ident name))) =
        return ["    ; Incr stmt"]
    
    compileStmt (Decr _ (LVar _ (Ident name))) =
        return ["    ; Decr stmt"]
    
    compileStmt (Ret _ expr) = do
        (val, exprCode) <- compileExpr expr
        let retInstr = case val of
                In intVal -> "    ret i32 " ++ show intVal
                Bo boolVal -> "    ret i1 " ++ if boolVal then "1" else "0"
                Re reg -> "    ret i32 %" ++ show reg
                _ -> error "Unsupported return type"
        return $ exprCode ++ [retInstr]
    
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
