module StmtChecker where
    import AbsLatte
    import Common
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
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
        env' <- foldM insertVar env vars
        return (env', Nothing)
        where
            insertVar :: Env -> Item -> TypeCheckerMonad Env
            insertVar env (NoInit pos (Ident name)) = do
                if Data.Map.member (Ident name) env then
                    throwError ("Variable " ++ name ++ " already declared, " ++ showPosition pos)
                else
                    return $ Data.Map.insert (Ident name) (toMyType typ) env
            insertVar env (Init pos (Ident name) expr) = do
                exprType <- getExprType expr
                if exprType /= toMyType typ then
                    throwError ("Type mismatch in initialization of " ++ name ++ ", " ++ showPosition pos)
                else
                    return $ Data.Map.insert (Ident name) (toMyType typ) env
    
    checkStmt (Ass _ (LVar pos (Ident name)) expr) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just varType -> do
                exprType <- getExprType expr
                if exprType /= varType then
                    throwError ("Type mismatch in assignment to " ++ name ++ ", " ++ showPosition pos)
                else
                    return (env, Nothing)
    
    checkStmt (Incr _ (LVar pos (Ident name))) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just varType -> do
                if varType /= MyInt then
                    throwError ("Variable " ++ name ++ " is not an integer, " ++ showPosition pos)
                else
                    return (env, Nothing)
    
    checkStmt (Decr _ (LVar pos (Ident name))) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just varType -> do
                if varType /= MyInt then
                    throwError ("Variable " ++ name ++ " is not an integer, " ++ showPosition pos)
                else
                    return (env, Nothing)
    
    checkStmt (Ret pos expr) = do
        env <- ask
        exprType <- getExprType expr
        return (env, Just (exprType, pos))
    
    checkStmt (VRet pos) = do
        env <- ask
        return (env, Just (MyVoid, pos))
    
    checkStmt (Cond pos expr stmt) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'if' statement must be a bool, " ++ showPosition pos)
        else do
            (_, _) <- local (const env) (checkStmt stmt)
            return (env, Nothing)
    
    checkStmt (CondElse pos expr stmt1 stmt2) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'if' statement must be a bool, " ++ showPosition pos)
        else do
            (_, _) <- local (const env) (checkStmt stmt1)
            (_, _) <- local (const env) (checkStmt stmt2)
            return (env, Nothing)
    
    checkStmt (While pos expr stmt) = do
        env <- ask
        exprType <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'while' statement must be a bool, " ++ showPosition pos)
        else do
            (_, _) <- local (const env) (checkStmt stmt)
            return (env, Nothing)
    
    checkStmt (SExp _ expr) = do
        env <- ask
        _ <- getExprType expr
        return (env, Nothing)
