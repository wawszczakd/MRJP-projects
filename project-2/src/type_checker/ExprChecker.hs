{-# LANGUAGE FlexibleContexts #-}

module ExprChecker where
    import AbsLatte
    import Control.Monad.Except
    import Control.Monad.Reader
    import Common
    import Data.Map
    
    getTypeFromEnv :: BNFC'Position -> Ident -> TypeCheckerMonad MyType
    getTypeFromEnv pos (Ident name) = do
        env <- ask
        case Data.Map.lookup (Ident name) env of
            Just (typ, _) -> return typ
            Nothing -> throwError ("Variable " ++ name ++ " is not declared, " ++ showPosition pos)
    
    getExprType :: Expr -> TypeCheckerMonad MyType
    
    getExprType (EVar pos (LVar _ name)) = getTypeFromEnv pos name
    
    getExprType (ELitInt _ _) = return MyInt
    
    getExprType (ELitTrue _) = return MyBool
    
    getExprType (ELitFalse _) = return MyBool
    
    getExprType (EApp pos (LVar _ (Ident name)) args) = do
        funType <- getTypeFromEnv pos (Ident name)
        case funType of
            MyFun typ expectedArgs -> do
                argsTypes <- mapM getExprType args
                if expectedArgs /= argsTypes then
                    throwError ("Types do not match, " ++ showPosition pos)
                else
                    return typ
            _ -> throwError (name ++ " is not callable, " ++ showPosition pos)
    
    getExprType (EString _ _) = return MyStr
    
    getExprType (Neg pos expr) = do
        exprType <- getExprType expr
        case exprType of
            MyInt -> return MyInt
            _ -> throwError ("'-' requires operand to be int, " ++ showPosition pos)
    
    getExprType (Not pos expr) = do
        exprType <- getExprType expr
        case exprType of
            MyBool -> return MyBool
            _ -> throwError ("'!' requires operand to be bool, " ++ showPosition pos)
    
    getExprType (EMul pos expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            Times _ -> handleMul exprType1 exprType2
            Div _   -> handleDiv exprType1 exprType2
            Mod _   -> handleMod exprType1 exprType2
        where
            handleMul MyInt MyInt = return MyInt
            handleMul _ _ = throwError ("'*' requires both operands to be ints, " ++ (showPosition pos))
            handleDiv MyInt MyInt = return MyInt
            handleDiv _ _ = throwError ("'/' requires both operands to be ints, " ++ (showPosition pos))
            handleMod MyInt MyInt = return MyInt
            handleMod _ _ = throwError ("'%' requires both operands to be ints, " ++ (showPosition pos))
    
    getExprType (EAdd pos expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            Plus _  -> handleAdd exprType1 exprType2
            Minus _ -> handleSub exprType1 exprType2
        where
            handleAdd MyInt MyInt = return MyInt
            handleAdd MyStr MyStr = return MyStr
            handleAdd _ _ = throwError ("'+' requires both operands to be ints or both to be strings, " ++ (showPosition pos))
            handleSub MyInt MyInt = return MyInt
            handleSub _ _ = throwError ("'-' requires both operands to be ints, " ++ (showPosition pos))

    
    getExprType (ERel pos expr1 op expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        case op of
            LTH _    -> checkRelOp "<" exprType1 exprType2
            LE _     -> checkRelOp "<=" exprType1 exprType2
            GTH _    -> checkRelOp ">" exprType1 exprType2
            GE _     -> checkRelOp ">=" exprType1 exprType2
            EQU _     -> checkEquality exprType1 exprType2
            NE _    -> checkEquality exprType1 exprType2
        where
            checkRelOp :: String -> MyType -> MyType -> TypeCheckerMonad MyType
            checkRelOp opName MyInt MyInt = return MyBool
            checkRelOp opName _ _ = throwError (opName ++ " requires both operands to be ints, " ++ showPosition pos)
            checkEquality :: MyType -> MyType -> TypeCheckerMonad MyType
            checkEquality MyInt MyInt = return MyBool
            checkEquality MyStr MyStr = return MyBool
            checkEquality MyBool MyBool = return MyBool
            checkEquality _ _ = throwError ("'==' or '!=' requires both operands to be of the same type, " ++ showPosition pos)
    
    getExprType (EAnd pos expr1 expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        handleLogicalOp pos exprType1 exprType2
    
    getExprType (EOr pos expr1 expr2) = do
        exprType1 <- getExprType expr1
        exprType2 <- getExprType expr2
        handleLogicalOp pos exprType1 exprType2
    
    handleLogicalOp :: BNFC'Position -> MyType -> MyType -> TypeCheckerMonad MyType
    handleLogicalOp pos MyBool MyBool = return MyBool
    handleLogicalOp pos _ _ = throwError ("Logical operations require both operands to be bools, " ++ (showPosition pos))
