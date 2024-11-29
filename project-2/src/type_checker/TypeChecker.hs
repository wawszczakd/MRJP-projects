module TypeChecker where
    import AbsLatte
    import Common
    import Control.Monad
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
    import Data.List
    import StmtChecker
    
    checkProgram :: Program -> TypeCheckerMonad ()
    checkProgram (Prog _ topDefs) = do
        let env = Data.Map.fromList [
                (Ident "readInt", MyFun MyInt []),
                (Ident "readString", MyFun MyStr []),
                (Ident "readBool", MyFun MyBool []),
                (Ident "printInt", MyFun MyVoid [MyInt]),
                (Ident "printString", MyFun MyVoid [MyStr]),
                (Ident "printBool", MyFun MyVoid [MyBool])]
        env' <- local (const env) (getEnv topDefs)
        local (const env') (checkTopDefs topDefs)
    
    getEnv :: [TopDef] -> TypeCheckerMonad Env
    getEnv [] = do
        env <- ask
        let tmp = Data.Map.lookup (Ident "main") env
        case tmp of
            Nothing -> throwError "'main' is not defined"
            Just (MyFun MyInt []) -> return env
            _ -> throwError "'main' must be a no-argument function that returns int"
    getEnv (topDef : topDefs) = do
        env <- insertTopDef topDef
        local (const env) (getEnv topDefs)
    
    checkTopDefs :: [TopDef] -> TypeCheckerMonad ()
    checkTopDefs [] = return ()
    checkTopDefs (topDef : topDefs) = do
        checkTopDef topDef
        checkTopDefs topDefs
    
    checkArgsNames :: Maybe (Int, Int) -> [Arg] -> Bool
    checkArgsNames pos args =
        let
            argsNames = Prelude.map (\(Ar _ _ name) -> name) args
        in
            Data.List.nub argsNames == argsNames
    
    checkArgsTypes :: Maybe (Int, Int) -> [Arg] -> Bool
    checkArgsTypes pos args =
        let
            argsTypes = Prelude.map (\(Ar _ typ _) -> toMyType typ) args
        in
            notElem MyVoid argsTypes
    
    insertTopDef :: TopDef -> TypeCheckerMonad Env
    insertTopDef (TopFunDef _ (FnDef pos typ name args block)) = do
        env <- ask
        if not (checkArgsNames pos args) then
            throwError ("Function args names must be pairwise distinct, " ++ showPosition pos)
        else if not (checkArgsTypes pos args) then
            throwError ("Argument cannot be of type void, " ++ showPosition pos)
        else do
            let expectedType = toMyType typ
                env' = Data.Map.insert name (MyFun expectedType (Prelude.map argToType args)) env
            return env'
    
    checkTopDef :: TopDef -> TypeCheckerMonad ()
    checkTopDef (TopFunDef _ (FnDef pos typ name args block)) = do
        env <- ask
        env' <- foldM insertArg env args
        ret <- local (const env') (checkBlock (toMyType typ) block)
        if ret == False then
            Control.Monad.when (toMyType typ /= MyVoid) $
                throwError ("No return, " ++ showPosition pos)
        else
            return ()
        where
            insertArg :: Env -> Arg -> TypeCheckerMonad Env
            insertArg env (Ar _ argType (Ident name)) = do
                if Data.Map.member (Ident name) env then
                    throwError ("Argument " ++ name ++ " already declared, " ++ showPosition pos)
                else
                    return $ Data.Map.insert (Ident name) (toMyType argType) env
    
    checkBlock :: MyType -> Block -> TypeCheckerMonad Bool
    checkBlock retType (Blck _ stmts) = do
        env <- ask
        local (const env) (checkStmts retType stmts)
