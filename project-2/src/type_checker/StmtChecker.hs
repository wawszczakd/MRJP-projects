module StmtChecker where
    import AbsLatte
    import Common
    import Control.Monad.Except
    import Control.Monad.Reader
    import ExprChecker
    
    checkStmts :: [Stmt] -> TypeCheckerMonad (Maybe (MyType, BNFC'Position))
    
    checkStmts [] = do
        return Nothing
    
    checkStmts (stmt : stmts) = do
        env <- ask
        (env', ret1) <- local (const env) (checkStmt stmt)
        ret2 <- local (const env') (checkStmts stmts)
        case (ret1, ret2) of
            (Nothing, Nothing) -> return Nothing
            (Just ret, Nothing) -> return (Just ret)
            (Nothing, Just ret) -> return (Just ret)
            (Just (typ1, pos1), Just (typ2, pos2)) ->
                if typ1 /= typ2 then
                    throwError ("Return types do not match, " ++ showPosition pos2)
                else
                    return (Just (typ1, pos1))
    
    checkStmt :: Stmt -> TypeCheckerMonad (Env, Maybe (MyType, BNFC'Position))
    
    checkStmt (Empty _) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (BStmt _ (Blck _ stmts)) = do
        env <- ask
        ret <- local (const env) (checkStmts stmts)
        return (env, ret)
    
    checkStmt (Decl _ typ vars) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (Ass _ lval expr) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (Incr _ lval) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (Decr _ lval) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (Ret _ expr) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (VRet pos) = do
        env <- ask
        return (env, Just (MyVoid, pos))
    
    checkStmt (Cond _ expr stmt) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (CondElse _ expr stmt1 stmt2) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (While _ expr stmt) = do
        env <- ask
        return (env, Nothing)
    
    checkStmt (SExp _ expr) = do
        env <- ask
        return (env, Nothing)
