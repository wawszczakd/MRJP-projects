module TypeChecker where
    import AbsLatte
    import Control.Monad
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
    import Data.List
    import StmtChecker
    import UtilsTypeChecker
    
    checkProgram :: Program -> TypeCheckerMonad ()
    checkProgram (Prog _ topDefs) = do
        let funEnv = Data.Map.fromList [
                (Ident "printInt", MyFun MyVoid [MyInt]),
                (Ident "printString", MyFun MyVoid [MyStr]),
                (Ident "error", MyFun MyVoid []),
                (Ident "readInt", MyFun MyInt []),
                (Ident "readString", MyFun MyStr [])]
        newEnv <- local (const (funEnv, Data.Map.empty)) (getFunEnv topDefs)
        local (const newEnv) (checkTopDefs topDefs)
    
    getFunEnv :: [TopDef] -> TypeCheckerMonad Env
    getFunEnv [] = do
        (funEnv, varEnv) <- ask
        let tmp = Data.Map.lookup (Ident "main") funEnv
        case tmp of
            Nothing -> throwError "'main' is not defined"
            Just (MyFun MyInt []) -> return (funEnv, varEnv)
            _ -> throwError "'main' must be a no-argument function that returns int"
    getFunEnv (topDef : topDefs) = do
        env <- insertTopDef topDef
        local (const env) (getFunEnv topDefs)
    
    checkTopDefs :: [TopDef] -> TypeCheckerMonad ()
    checkTopDefs [] = return ()
    checkTopDefs (topDef : topDefs) = do
        checkTopDef topDef
        checkTopDefs topDefs
    
    insertTopDef :: TopDef -> TypeCheckerMonad Env
    insertTopDef (TopFunDef _ (FnDef pos typ name args block)) = do
        (funEnv, varEnv) <- ask
        if not (checkArgsNames pos args) then
            throwError ("Function args names must be pairwise distinct, " ++ showPosition pos)
        else if not (checkArgsTypes pos args) then
            throwError ("Argument cannot be of type void, " ++ showPosition pos)
        else do
            let expectedType = toMyType typ
                newFunEnv = Data.Map.insert name (MyFun expectedType (Prelude.map argToType args)) funEnv
            return (newFunEnv, varEnv)
        where
            checkArgsNames :: Maybe (Int, Int) -> [Arg] -> Bool
            checkArgsNames pos args =
                let argsNames = Prelude.map (\(Ar _ _ name) -> name) args in
                Data.List.nub argsNames == argsNames
            
            checkArgsTypes :: Maybe (Int, Int) -> [Arg] -> Bool
            checkArgsTypes pos args =
                let argsTypes = Prelude.map (\(Ar _ typ _) -> toMyType typ) args in
                notElem MyVoid argsTypes
    
    checkTopDef :: TopDef -> TypeCheckerMonad ()
    checkTopDef (TopFunDef _ (FnDef pos typ name args block)) = do
        (funEnv, varEnv) <- ask
        (newFunEnv, newVarEnv) <- foldM insertArg (funEnv, varEnv) args
        ret <- local (const (newFunEnv, newVarEnv)) (checkBlock 1 (toMyType typ) block)
        unless ret $
            Control.Monad.when (toMyType typ /= MyVoid) $
                throwError ("No return, " ++ showPosition pos)
        where
            insertArg :: Env -> Arg -> TypeCheckerMonad Env
            insertArg (funEnv, varEnv) (Ar _ argType name) =
                return (funEnv, Data.Map.insert name (toMyType argType, 1) varEnv)
    
    checkBlock :: Integer -> MyType -> Block -> TypeCheckerMonad Bool
    checkBlock depth retType (Blck _ stmts) = do
        env <- ask
        local (const env) (checkStmts depth retType stmts)
