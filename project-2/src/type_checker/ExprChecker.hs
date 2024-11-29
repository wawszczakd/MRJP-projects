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
    
    getExprType :: Expr -> TypeCheckerMonad (MyType, ValueState)
    
    getExprType (EVar pos (LVar _ name)) = do
        typ <- getTypeFromEnv pos name
        return (typ, Unknown)
    
    getExprType (ELitInt _ val) = return (MyInt, FixedInt val)
    
    getExprType (ELitTrue _) = return (MyBool, FixedBool True)
    
    getExprType (ELitFalse _) = return (MyBool, FixedBool False)
    
    getExprType (EApp pos (LVar _ (Ident name)) args) = do
        funType <- getTypeFromEnv pos (Ident name)
        case funType of
            MyFun typ expectedArgs -> do
                argsTypes <- mapM (fmap fst . getExprType) args
                if expectedArgs /= argsTypes then
                    throwError ("Types do not match, " ++ showPosition pos)
                else
                    return (typ, Unknown)
            _ -> throwError (name ++ " is not callable, " ++ showPosition pos)
    
    getExprType (EString _ val) = return (MyStr, FixedString val)
    
    getExprType (Neg pos expr) = do
        (exprType, exprVal) <- getExprType expr
        case exprType of
            MyInt -> return (MyInt, case exprVal of
                                        FixedInt val -> FixedInt (-val)
                                        _ -> Unknown)
            _ -> throwError ("'-' requires operand to be int, " ++ showPosition pos)
    
    getExprType (Not pos expr) = do
        (exprType, exprVal) <- getExprType expr
        case exprType of
            MyBool -> return (MyBool, case exprVal of
                                          FixedBool True -> FixedBool False
                                          FixedBool False -> FixedBool True
                                          _ -> Unknown)
            _ -> throwError ("'!' requires operand to be bool, " ++ showPosition pos)
    
    getExprType (EMul pos expr1 op expr2) = do
        (exprType1, val1) <- getExprType expr1
        (exprType2, val2) <- getExprType expr2
        case op of
            Times _ -> handleMul exprType1 val1 exprType2 val2
            Div _   -> handleDiv exprType1 val1 exprType2 val2
            Mod _   -> handleMod exprType1 val1 exprType2 val2
        where
            handleMul MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyInt, FixedInt (v1 * v2))
            handleMul MyInt _ MyInt _ = return (MyInt, Unknown)
            handleMul _ _ _ _ = throwError ("'*' requires both operands to be ints, " ++ showPosition pos)
            
            handleDiv MyInt (FixedInt v1) MyInt (FixedInt v2)
                | v2 /= 0  = return (MyInt, FixedInt (v1 `div` v2))
                | otherwise = throwError ("Division by zero, " ++ showPosition pos)
            handleDiv MyInt _ MyInt _ = return (MyInt, Unknown)
            handleDiv _ _ _ _ = throwError ("'/' requires both operands to be ints, " ++ showPosition pos)
            
            handleMod MyInt (FixedInt v1) MyInt (FixedInt v2)
                | v2 /= 0  = return (MyInt, FixedInt (v1 `mod` v2))
                | otherwise = throwError ("Modulo by zero, " ++ showPosition pos)
            handleMod MyInt _ MyInt _ = return (MyInt, Unknown)
            handleMod _ _ _ _ = throwError ("'%' requires both operands to be ints, " ++ showPosition pos)
    
    getExprType (EAdd pos expr1 op expr2) = do
        (exprType1, val1) <- getExprType expr1
        (exprType2, val2) <- getExprType expr2
        case op of
            Plus _  -> handleAdd exprType1 val1 exprType2 val2
            Minus _ -> handleSub exprType1 val1 exprType2 val2
        where
            handleAdd MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyInt, FixedInt (v1 + v2))
            handleAdd MyInt _ MyInt _ = return (MyInt, Unknown)
            handleAdd MyStr (FixedString s1) MyStr (FixedString s2) = return (MyStr, FixedString (s1 ++ s2))
            handleAdd MyStr _ MyStr _ = return (MyStr, Unknown)
            handleAdd _ _ _ _ = throwError ("'+' requires both operands to be ints or both to be strings, " ++ showPosition pos)
            
            handleSub MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyInt, FixedInt (v1 - v2))
            handleSub MyInt _ MyInt _ = return (MyInt, Unknown)
            handleSub _ _ _ _ = throwError ("'-' requires both operands to be ints, " ++ showPosition pos)
    
    getExprType (ERel pos expr1 op expr2) = do
        (exprType1, val1) <- getExprType expr1
        (exprType2, val2) <- getExprType expr2
        case op of
            LTH _ -> checkRelOp "<" exprType1 val1 exprType2 val2
            LE _  -> checkRelOp "<=" exprType1 val1 exprType2 val2
            GTH _ -> checkRelOp ">" exprType1 val1 exprType2 val2
            GE _  -> checkRelOp ">=" exprType1 val1 exprType2 val2
            EQU _ -> checkEqual exprType1 val1 exprType2 val2
            NE _  -> checkNotEqual exprType1 val1 exprType2 val2
        where
            checkRelOp "<" MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyBool, FixedBool (v1 < v2))
            checkRelOp "<=" MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyBool, FixedBool (v1 <= v2))
            checkRelOp ">" MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyBool, FixedBool (v1 > v2))
            checkRelOp ">=" MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyBool, FixedBool (v1 >= v2))
            checkRelOp opName MyInt _ MyInt _ = return (MyBool, Unknown)
            checkRelOp opName _ _ _ _ = throwError ("'" ++ opName ++ "' requires both operands to be ints, " ++ showPosition pos)
            
            checkEqual MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyBool, FixedBool (v1 == v2))
            checkEqual MyBool (FixedBool v1) MyBool (FixedBool v2) = return (MyBool, FixedBool (v1 == v2))
            checkEqual MyStr (FixedString v1) MyStr (FixedString v2) = return (MyBool, FixedBool (v1 == v2))
            checkEqual MyInt _ MyInt _ = return (MyBool, Unknown)
            checkEqual MyBool _ MyBool _ = return (MyBool, Unknown)
            checkEqual MyStr _ MyStr _ = return (MyBool, Unknown)
            checkEqual _ _ _ _ = throwError ("'==' requires both operands to be of the same type, " ++ showPosition pos)
            
            checkNotEqual MyInt (FixedInt v1) MyInt (FixedInt v2) = return (MyBool, FixedBool (v1 /= v2))
            checkNotEqual MyBool (FixedBool v1) MyBool (FixedBool v2) = return (MyBool, FixedBool (v1 /= v2))
            checkNotEqual MyStr (FixedString v1) MyStr (FixedString v2) = return (MyBool, FixedBool (v1 /= v2))
            checkNotEqual MyInt _ MyInt _ = return (MyBool, Unknown)
            checkNotEqual MyBool _ MyBool _ = return (MyBool, Unknown)
            checkNotEqual MyStr _ MyStr _ = return (MyBool, Unknown)
            checkNotEqual _ _ _ _ = throwError ("'!=' requires both operands to be of the same type, " ++ showPosition pos)
    
    getExprType (EAnd pos expr1 expr2) = do
        (exprType1, val1) <- getExprType expr1
        (exprType2, val2) <- getExprType expr2
        handleLogicalOp pos "and" exprType1 val1 exprType2 val2
    
    getExprType (EOr pos expr1 expr2) = do
        (exprType1, val1) <- getExprType expr1
        (exprType2, val2) <- getExprType expr2
        handleLogicalOp pos "or" exprType1 val1 exprType2 val2
    
    handleLogicalOp :: BNFC'Position -> String -> MyType -> ValueState -> MyType -> ValueState -> TypeCheckerMonad (MyType, ValueState)
    handleLogicalOp pos "and" MyBool (FixedBool v1) MyBool (FixedBool v2) = do
        let result = FixedBool (v1 && v2)
        return (MyBool, result)
    
    handleLogicalOp pos "or" MyBool (FixedBool v1) MyBool (FixedBool v2) = do
        let result = FixedBool (v1 || v2)
        return (MyBool, result)
    
    handleLogicalOp pos _ MyBool _ MyBool _ = return (MyBool, Unknown)
    
    handleLogicalOp pos _ _ _ _ _ = throwError ("Logical operations require both operands to be bools, " ++ showPosition pos)
