{-# LANGUAGE NamedFieldPuns #-}

module ExprCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import UtilsCompiler
    
    compileExpr :: Expr -> CompilerMonad (ExprVal, [String])
    
    compileExpr (EVar _ (LVar _ name)) = do
        (_, _, env, store) <- get
        let
            Just (VarEntry loc) = Data.Map.lookup name env
            Just reg = Data.Map.lookup loc store
        return (Re reg, [])
    
    compileExpr (ELitInt _ val) =
        return (In val, [])
    
    compileExpr (ELitTrue _) =
        return (Bo True, [])
    
    compileExpr (ELitFalse _) =
        return (Bo False, [])
    
    compileExpr (EApp _ (LVar _ (Ident name)) args) = do
        (_, _, env, _) <- get
        let Just (FuncEntry retType) = Data.Map.lookup (Ident name) env
        (argVals, instrs) <- compileArgs args
        (nextLoc, nextReg, _, store) <- get
        let
            argList = intercalate ", " $ Data.List.map formatArg argVals
            callInstr
                | retType == "void" = "    call " ++ retType ++ " @" ++ name ++ "(" ++ argList ++ ")"
                | otherwise =
                    let callReg = nextReg
                    in "    %" ++ show callReg ++ " = call " ++ retType ++ " @" ++ name ++ "(" ++ argList ++ ")"
        
        if retType /= "void" then
            put (nextLoc, nextReg + 1, env, store)
        else
            put (nextLoc, nextReg, env, store)
        
        return (if retType == "void" then Re (-1) else Re nextReg, instrs ++ [callInstr])
        where
            compileArgs :: [Expr] -> CompilerMonad ([ExprVal], [String])
            compileArgs [] = return ([], [])
            compileArgs (arg:rest) = do
                (argVal, argCode) <- compileExpr arg
                (restVals, restCodes) <- compileArgs rest
                return (argVal : restVals, argCode ++ restCodes)
            
            formatArg :: ExprVal -> String
            formatArg (In val) = "i32 " ++ show val
            formatArg (Bo True) = "i1 1"
            formatArg (Bo False) = "i1 0"
            formatArg (Re reg) = "i32 %" ++ show reg

