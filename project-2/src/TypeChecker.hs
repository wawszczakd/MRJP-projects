module TypeChecker where
    import AbsLatte
    import Common
    import Control.Monad
    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.State
    import Data.Map as Map
    import Data.Maybe
    import qualified Data.List as List
    
    data MyType = MyInt | MyStr | MyBool | MyVoid | MyFun MyType [MyType] deriving (Eq, Show)
    
    toMyType :: Type -> MyType
    toMyType (Int _) = MyInt
    toMyType (Str _) = MyStr
    toMyType (Bool _) = MyBool
    toMyType (Void _) = MyVoid
    
    argToType :: Arg -> MyType
    argToType (Ar _ typ _) = toMyType typ
    
    type Env = Map.Map Ident MyType
    
    type TypeCheckerMonad = ReaderT Env (ExceptT String IO)
    
    checkProgram :: Program -> TypeCheckerMonad ()
    checkProgram (Prog _ topDefs) = do
        env <- getEnv topDefs
        local (const env) (checkTopDefs topDefs)
    
    getEnv :: [TopDef] -> TypeCheckerMonad Env
    getEnv [] = do
        env <- ask
        let tmp = Map.lookup (Ident "main") env
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
            List.nub argsNames == argsNames
    
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
                env' = Map.insert name (MyFun expectedType (Prelude.map argToType args)) env
            return env'
    
    checkTopDef :: TopDef -> TypeCheckerMonad ()
    checkTopDef (TopFunDef _ (FnDef pos typ name args block)) = do
        env <- ask
        ret <- local (const env) (checkBlock block)
        case ret of
            Nothing ->
                Control.Monad.when (toMyType typ /= MyVoid) $ throwError ("No return, " ++ showPosition pos)
            (Just (typ', _)) ->
                Control.Monad.when (toMyType typ /= typ') $ throwError ("Wrong return type, " ++ showPosition pos)
    
    checkBlock :: Block -> TypeCheckerMonad (Maybe (MyType, BNFC'Position))
    checkBlock (Blck _ stmts) = do
        env <- ask
        local (const env) (checkStmts stmts)
    
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
                if typ1 /= typ2
                then throwError ("Return types do not match, " ++ showPosition pos2)
                else return (Just (typ1, pos1))
    
    checkStmt :: Stmt -> TypeCheckerMonad (Env, Maybe (MyType, BNFC'Position))
    checkStmt (Empty _) = do
        env <- ask
        return (env, Nothing)
    checkStmt (BStmt _ block) = do
        env <- ask
        return (env, Nothing)
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
