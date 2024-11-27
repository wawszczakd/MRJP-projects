{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where
    import AbsLatte
    import Common
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except
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
    checkProgram (Prog _ topDefs) =
        go topDefs
        where
            go :: [TopDef] -> TypeCheckerMonad ()
            go [] = do
                env <- ask
                let tmp = Map.lookup (Ident "main") env
                case tmp of
                    Nothing -> throwError "'main' is not defined"
                    Just (MyFun MyInt []) -> return ()
                    _ -> throwError "'main' must be a no-argument function that returns int"
            go (x:xs) = do
                env <- checkTopDef x
                local (const env) (go xs)
    
    checkArgsNames :: Maybe (Int, Int) -> [Arg] -> Bool
    checkArgsNames pos args =
        let
            argsNames = Prelude.map (\(Ar _ _ name) -> name) args
        in
            if List.nub argsNames == argsNames then
                True
            else
                False
    
    checkArgsTypes :: Maybe (Int, Int) -> [Arg] -> Bool
    checkArgsTypes pos args =
        let
            argsTypes = Prelude.map (\(Ar _ typ _) -> toMyType typ) args
        in
            if not (elem MyVoid argsTypes) then
                True
            else
                False
    
    checkTopDef :: TopDef -> TypeCheckerMonad Env
    
    checkTopDef (TopFunDef _ (FnDef pos typ name args block)) = do
        env <- ask
        if not (checkArgsNames pos args) then
            throwError ("Function args names must be pairwise distinct, " ++ (showPosition pos))
        else if not (checkArgsTypes pos args) then
            throwError ("Argument cannot be of type void, " ++ (showPosition pos))
        else do
            let expectedType = toMyType typ
                env' = Map.insert name (MyFun expectedType (Prelude.map argToType args)) env
            return env'
