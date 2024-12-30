module StmtChecker where
    import AbsLatte
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
    import ExprChecker
    import UtilsTypeChecker
    
    checkStmts :: Integer -> MyType -> [Stmt] -> TypeCheckerMonad Bool
    
    checkStmts _ retType [] = return False
    
    checkStmts depth retType (stmt : stmts) = do
        env <- ask
        (env', ret1) <- local (const env) (checkStmt depth retType stmt)
        ret2 <- local (const env') (checkStmts depth retType stmts)
        return (ret1 || ret2)
    
    checkStmt :: Integer -> MyType -> Stmt -> TypeCheckerMonad (Env, Bool)
    
    checkStmt _ _ (Empty _) = do
        env <- ask
        return (env, False)
    
    checkStmt depth retType (BStmt _ (Blck _ stmts)) = do
        env <- ask
        ret <- local (const env) (checkStmts (depth + 1) retType stmts)
        return (env, ret)
    
    checkStmt depth _ (Decl _ typ vars) = do
        env <- ask
        env' <- foldM (insertVar depth) env vars
        return (env', False)
        where
            insertVar :: Integer -> Env -> Item -> TypeCheckerMonad Env
            insertVar depth env (NoInit pos (Ident name)) =
                checkAlreadyDeclared depth env (Ident name) pos >>
                    return (Data.Map.insert (Ident name) (toMyType typ, depth) env)
            insertVar depth env (Init pos (Ident name) expr) =
                checkAlreadyDeclared depth env (Ident name) pos >> (do
                    (exprType, _) <- local (const env) (getExprType expr)
                    if exprType /= toMyType typ then
                        throwError ("Type mismatch in initialization of " ++ name ++ ", " ++ showPosition pos)
                    else
                        return $ Data.Map.insert (Ident name) (toMyType typ, depth) env)
            
            checkAlreadyDeclared :: Integer -> Env -> Ident -> BNFC'Position -> TypeCheckerMonad ()
            checkAlreadyDeclared depth env (Ident name) pos =
                case Data.Map.lookup (Ident name) env of
                    Just (_, depth') ->
                        when (depth == depth') $
                            throwError ("Variable " ++ name ++ " already declared, " ++ showPosition pos)
                    Nothing -> return ()
    
    checkStmt _ _ (Ass _ (LVar pos (Ident name)) expr) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just (varType, _) -> do
                (exprType, _) <- getExprType expr
                if exprType /= varType then
                    throwError ("Type mismatch in assignment to " ++ name ++ ", " ++ showPosition pos)
                else
                    return (env, False)
    
    checkStmt _ _ (Incr _ (LVar pos (Ident name))) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just (varType, _) -> do
                if varType /= MyInt then
                    throwError ("Variable " ++ name ++ " is not an integer, " ++ showPosition pos)
                else
                    return (env, False)
    
    checkStmt _ _ (Decr _ (LVar pos (Ident name))) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
            Just (varType, _) -> do
                if varType /= MyInt then
                    throwError ("Variable " ++ name ++ " is not an integer, " ++ showPosition pos)
                else
                    return (env, False)
    
    checkStmt _ retType (Ret pos expr) = do
        env <- ask
        (exprType, _) <- getExprType expr
        if exprType /= retType then
            throwError ("Wrong return type, " ++ showPosition pos)
        else
            return (env, True)
    
    checkStmt _ retType (VRet pos) = do
        env <- ask
        if retType /= MyVoid then
            throwError ("Wrong return type, " ++ showPosition pos)
        else
            return (env, True)
    
    checkStmt depth retType (Cond pos expr stmt) = do
        env <- ask
        (exprType, exprVal) <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'if' statement must be a bool, " ++ showPosition pos)
        else do
            (_, ret) <- local (const env) (checkStmt (depth + 1) retType stmt)
            let isRet = case exprVal of
                    FixedBool True -> ret
                    _              -> False
            return (env, isRet)
    
    checkStmt depth retType (CondElse pos expr stmt1 stmt2) = do
        env <- ask
        (exprType, exprVal) <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'if' statement must be a bool, " ++ showPosition pos)
        else do
            (_, ret1) <- local (const env) (checkStmt (depth + 1) retType stmt1)
            (_, ret2) <- local (const env) (checkStmt (depth + 1) retType stmt2)
            let isRet = case exprVal of
                    FixedBool True  -> ret1
                    FixedBool False -> ret2
                    _               -> ret1 && ret2
            return (env, isRet)
    
    checkStmt depth retType (While pos expr stmt) = do
        env <- ask
        (exprType, exprVal) <- getExprType expr
        if exprType /= MyBool then
            throwError ("Condition in 'while' statement must be a bool, " ++ showPosition pos)
        else do
            (_, ret) <- local (const env) (checkStmt (depth + 1) retType stmt)
            let isRet = case exprVal of
                    FixedBool True -> ret
                    _              -> False
            return (env, isRet)
    
    checkStmt _ _ (SExp _ expr) = do
        env <- ask
        _ <- getExprType expr
        return (env, False)
