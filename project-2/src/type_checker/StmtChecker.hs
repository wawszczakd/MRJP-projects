module StmtChecker where
    import AbsLatte
    import Common
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
    import ExprChecker
    
    checkStmts :: MyType -> [Stmt] -> TypeCheckerMonad Bool
    
    checkStmts retType [] = do
        return False
    
    checkStmts retType (stmt : stmts) = do
        env <- ask
        (env', ret1) <- local (const env) (checkStmt retType stmt)
        ret2 <- local (const env') (checkStmts retType stmts)
        return (ret1 || ret2)
    
    checkStmt :: MyType -> Stmt -> TypeCheckerMonad (Env, Bool)
    
    checkStmt _ (Empty _) = do
        env <- ask
        return (env, False)
    
    checkStmt retType (BStmt _ (Blck _ stmts)) = do
        env <- ask
        ret <- local (const env) (checkStmts retType stmts)
        return (env, ret)
    
    checkStmt _ (Decl _ typ vars) = do
        env <- ask
        env' <- foldM insertVar env vars
        return (env', False)
        where
            insertVar :: Env -> Item -> TypeCheckerMonad Env
            insertVar env (NoInit pos (Ident name)) = do
                case Data.Map.lookup (Ident name) env of
                    Just _ -> throwError ("Variable " ++ name ++ " already declared, " ++ showPosition pos)
                    Nothing -> return $ Data.Map.insert (Ident name) (toMyType typ) env
            insertVar env (Init pos (Ident name) expr) = do
                case Data.Map.lookup (Ident name) env of
                    Just _ -> throwError ("Variable " ++ name ++ " already declared, " ++ showPosition pos)
                    Nothing -> do
                        exprType <- getExprType expr
                        if exprType /= toMyType typ then
                            throwError ("Type mismatch in initialization of " ++ name ++ ", " ++ showPosition pos)
                        else
                            return $ Data.Map.insert (Ident name) (toMyType typ) env
    
    checkStmt _ (Ass _ (LVar pos (Ident name)) expr) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just varType -> do
                exprType <- getExprType expr
                if exprType /= varType then
                    throwError ("Type mismatch in assignment to " ++ name ++ ", " ++ showPosition pos)
                else
                    return (env, False)
    
    checkStmt _ (Incr _ (LVar pos (Ident name))) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just varType -> do
                if varType /= MyInt then
                    throwError ("Variable " ++ name ++ " is not an integer, " ++ showPosition pos)
                else
                    return (env, False)
    
    checkStmt _ (Decr _ (LVar pos (Ident name))) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just varType -> do
                if varType /= MyInt then
                    throwError ("Variable " ++ name ++ " is not an integer, " ++ showPosition pos)
                else
                    return (env, False)
    
    checkStmt retType (Ret pos expr) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= retType then
            throwError ("Wrong return type, " ++ showPosition pos)
        else
            return (env, True)
    
    checkStmt retType (VRet pos) = do
        env <- ask
        if retType /= MyVoid then
            throwError ("Wrong return type, " ++ showPosition pos)
        else
            return (env, True)
    
    checkStmt retType (Cond pos expr stmt) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'if' statement must be a bool, " ++ showPosition pos)
        else do
            (_, _) <- local (const env) (checkStmt retType stmt)
            return (env, False)
    
    checkStmt retType (CondElse pos expr stmt1 stmt2) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'if' statement must be a bool, " ++ showPosition pos)
        else do
            (_, ret1) <- local (const env) (checkStmt retType stmt1)
            (_, ret2) <- local (const env) (checkStmt retType stmt2)
            return (env, ret1 && ret2)
    
    checkStmt retType (While pos expr stmt) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'while' statement must be a bool, " ++ showPosition pos)
        else do
            (_, _) <- local (const env) (checkStmt retType stmt)
            return (env, False)
    
    checkStmt _ (SExp _ expr) = do
        env <- ask
        _ <- getExprType expr
        return (env, False)
