{-# LANGUAGE NamedFieldPuns #-}

module ExprCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import UtilsCompiler
    
    compileExpr :: Expr -> CompilerMonad (ExprVal, [String])
    
    compileExpr (EVar _ (LVar _ name)) = do
        (_, _, (funEnv, varEnv), store) <- get
        let Just loc = Data.Map.lookup name varEnv
        let Just val = Data.Map.lookup loc store
        return (val, [])
    
    compileExpr (ELitInt _ val) =
        return (In val, [])
    
    compileExpr (ELitTrue _) =
        return (Bo True, [])
    
    compileExpr (ELitFalse _) =
        return (Bo False, [])
    
    compileExpr (EApp _ (LVar _ (Ident name)) args) = do
        (_, _, (funEnv, varEnv), _) <- get
        let Just retType = Data.Map.lookup (Ident name) funEnv
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
            put (nextLoc, nextReg + 1, (funEnv, varEnv), store)
        else
            put (nextLoc, nextReg, (funEnv, varEnv), store)
        
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
    
    compileExpr (Neg _ expr) = do
        (exprVal, instrs) <- compileExpr expr
        case exprVal of
            In val -> return (In (-val), instrs)
            Re reg -> do
                (nextLoc, nextReg, env, store) <- get
                let negInstr = "    %" ++ show nextReg ++ " = sub i32 0, %" ++ show reg
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs ++ [negInstr])
    
    compileExpr (Not _ expr) = do
        (exprVal, instrs) <- compileExpr expr
        case exprVal of
            Bo val -> return (Bo (not val), instrs)
            Re reg -> do
                (nextLoc, nextReg, env, store) <- get
                let notInstr = "    %" ++ show nextReg ++ " = xor i1 1, %" ++ show reg
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs ++ [notInstr])
    
    compileExpr (EMul _ expr1 op expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (In val1, In val2) ->
                case op of
                    Times _ -> return (In (val1 * val2), instrs1 ++ instrs2)
                    Div _ -> return (In (val1 `div` val2), instrs1 ++ instrs2)
                    Mod _ -> return (In (val1 `mod` val2), instrs1 ++ instrs2)
            
            (Re reg1, In val2) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = case op of
                                Times _ -> "    %" ++ show nextReg ++ " = mul i32 %" ++ show reg1 ++ ", " ++ show val2
                                Div _ -> "    %" ++ show nextReg ++ " = sdiv i32 %" ++ show reg1 ++ ", " ++ show val2
                                Mod _ -> "    %" ++ show nextReg ++ " = srem i32 %" ++ show reg1 ++ ", " ++ show val2
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs1 ++ instrs2 ++ [instr])
            
            (In val1, Re reg2) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = case op of
                                Times _ -> "    %" ++ show nextReg ++ " = mul i32 " ++ show val1 ++ ", %" ++ show reg2
                                Div _ -> "    %" ++ show nextReg ++ " = sdiv i32 " ++ show val1 ++ ", %" ++ show reg2
                                Mod _ -> "    %" ++ show nextReg ++ " = srem i32 " ++ show val1 ++ ", %" ++ show reg2
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs1 ++ instrs2 ++ [instr])
            
            (Re reg1, Re reg2) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = case op of
                                Times _ -> "    %" ++ show nextReg ++ " = mul i32 %" ++ show reg1 ++ ", %" ++ show reg2
                                Div _ -> "    %" ++ show nextReg ++ " = sdiv i32 %" ++ show reg1 ++ ", %" ++ show reg2
                                Mod _ -> "    %" ++ show nextReg ++ " = srem i32 %" ++ show reg1 ++ ", %" ++ show reg2
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs1 ++ instrs2 ++ [instr])
    
    compileExpr (EAdd _ expr1 op expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (In val1, In val2) ->
                case op of
                    Plus _  -> return (In (val1 + val2), instrs1 ++ instrs2)
                    Minus _ -> return (In (val1 - val2), instrs1 ++ instrs2)
            
            (Re reg1, In val2) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = case op of
                                Plus _  -> "    %" ++ show nextReg ++ " = add i32 %" ++ show reg1 ++ ", " ++ show val2
                                Minus _ -> "    %" ++ show nextReg ++ " = sub i32 %" ++ show reg1 ++ ", " ++ show val2
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs1 ++ instrs2 ++ [instr])
            
            (In val1, Re reg2) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = case op of
                                Plus _  -> "    %" ++ show nextReg ++ " = add i32 " ++ show val1 ++ ", %" ++ show reg2
                                Minus _ -> "    %" ++ show nextReg ++ " = sub i32 " ++ show val1 ++ ", %" ++ show reg2
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs1 ++ instrs2 ++ [instr])
            
            (Re reg1, Re reg2) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = case op of
                                Plus _  -> "    %" ++ show nextReg ++ " = add i32 %" ++ show reg1 ++ ", %" ++ show reg2
                                Minus _ -> "    %" ++ show nextReg ++ " = sub i32 %" ++ show reg1 ++ ", %" ++ show reg2
                put (nextLoc, nextReg + 1, env, store)
                return (Re nextReg, instrs1 ++ instrs2 ++ [instr])
