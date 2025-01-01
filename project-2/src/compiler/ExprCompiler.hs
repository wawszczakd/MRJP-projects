{-# LANGUAGE NamedFieldPuns #-}

module ExprCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import LLVMInstructions
    import UtilsCompiler
    
    compileExpr :: Expr -> CompilerMonad (LLVMVal, [LLVMInstr])
    
    compileExpr (EVar _ (LVar _ name)) = do
        (_, _, (funEnv, varEnv), store) <- get
        let Just loc = Data.Map.lookup name varEnv
        let Just val = Data.Map.lookup loc store
        return (val, [])
    
    compileExpr (ELitInt _ val) =
        return (IntVal val, [])
    
    compileExpr (ELitTrue _) =
        return (BoolVal True, [])
    
    compileExpr (ELitFalse _) =
        return (BoolVal False, [])
    
    compileExpr (EApp _ (LVar _ (Ident name)) args) = do
        (_, _, (funEnv, varEnv), _) <- get
        let Just retType = Data.Map.lookup (Ident name) funEnv
        (argVals, instrs) <- compileArgs args
        (nextLoc, nextReg, _, store) <- get
        let
            instr
                | retType == LLVMVoid = LLVMCall retType name argVals
                | otherwise = LLVMAss (LLVMReg nextReg) (LLVMCall retType name argVals)
        
        if retType /= LLVMVoid then
            put (nextLoc, nextReg + 1, (funEnv, varEnv), store)
        else
            put (nextLoc, nextReg, (funEnv, varEnv), store)
        
        return (if retType == LLVMVoid then RegVal LLVMVoid (LLVMReg (-1)) else RegVal retType (LLVMReg nextReg), instrs ++ [instr])
        where
            compileArgs :: [Expr] -> CompilerMonad ([LLVMValT], [LLVMInstr])
            compileArgs [] = return ([], [])
            compileArgs (arg : rest) = do
                (argVal, argCode) <- compileExpr arg
                (restVals, restCodes) <- compileArgs rest
                return ((toLLVMValT argVal) : restVals, argCode ++ restCodes)
    
    compileExpr (Neg _ expr) = do
        (exprVal, instrs) <- compileExpr expr
        case exprVal of
            IntVal val -> return (IntVal (-val), instrs)
            RegVal _ (LLVMReg reg) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = LLVMBin (LLVMReg nextReg) LLVMMinus LLVMInt (IntVal 0) (RegVal LLVMInt (LLVMReg reg))
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMInt (LLVMReg nextReg), instrs ++ [instr])
    
    compileExpr (Not _ expr) = do
        (exprVal, instrs) <- compileExpr expr
        case exprVal of
            BoolVal val -> return (BoolVal (not val), instrs)
            RegVal LLVMBool (LLVMReg reg) -> do
                (nextLoc, nextReg, env, store) <- get
                let notInstr = LLVMBin (LLVMReg nextReg) LLVMXor LLVMBool (BoolVal True) (RegVal LLVMBool (LLVMReg reg))
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMBool (LLVMReg nextReg), instrs ++ [notInstr])
    
    compileExpr (EMul _ expr1 op expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (IntVal val1, IntVal val2) ->
                case op of
                    Times _ -> return (IntVal (val1 * val2), instrs1 ++ instrs2)
                    Div _ -> return (IntVal (val1 `div` val2), instrs1 ++ instrs2)
                    Mod _ -> return (IntVal (val1 `mod` val2), instrs1 ++ instrs2)
            (lhs, rhs) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = LLVMBin (LLVMReg nextReg) (opToLLVM op) LLVMInt lhs rhs
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMInt (LLVMReg nextReg), instrs1 ++ instrs2 ++ [instr])
        where
            opToLLVM :: MulOp -> LLVMBinOp
            opToLLVM (Times _) = LLVMTimes
            opToLLVM (Div _)   = LLVMDiv
            opToLLVM (Mod _)   = LLVMMod
    
    compileExpr (EAdd _ expr1 op expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (IntVal val1, IntVal val2) ->
                case op of
                    Plus _  -> return (IntVal (val1 + val2), instrs1 ++ instrs2)
                    Minus _ -> return (IntVal (val1 - val2), instrs1 ++ instrs2)
            (lhs, rhs) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = LLVMBin (LLVMReg nextReg) (opToLLVM op) LLVMInt lhs rhs
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMInt (LLVMReg nextReg), instrs1 ++ instrs2 ++ [instr])
        where
            opToLLVM :: AddOp -> LLVMBinOp
            opToLLVM (Plus _)  = LLVMPlus
            opToLLVM (Minus _) = LLVMMinus
    
    compileExpr (ERel _ expr1 op expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (IntVal val1, IntVal val2) ->
                let result = case op of
                                LTH _ -> val1 < val2
                                LE _  -> val1 <= val2
                                GTH _ -> val1 > val2
                                GE _  -> val1 >= val2
                                EQU _ -> val1 == val2
                                NE _  -> val1 /= val2
                in return (BoolVal result, instrs1 ++ instrs2)
            (BoolVal val1, BoolVal val2) ->
                let result = case op of
                                EQU _ -> val1 == val2
                                NE _  -> val1 /= val2
                in return (BoolVal result, instrs1 ++ instrs2)
            (StrVal val1, StrVal val2) ->
                let result = case op of
                                EQU _ -> val1 == val2
                                NE _  -> val1 /= val2
                in return (BoolVal result, instrs1 ++ instrs2)
            (lhs, rhs) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = LLVMBin (LLVMReg nextReg) (opToLLVM op) (getLLVMType lhs) lhs rhs
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMBool (LLVMReg nextReg), instrs1 ++ instrs2 ++ [instr])
        where
            opToLLVM :: RelOp -> LLVMBinOp
            opToLLVM (LTH _) = LLVMLTH
            opToLLVM (LE _)  = LLVMLE
            opToLLVM (GTH _) = LLVMGTH
            opToLLVM (GE _)  = LLVMGE
            opToLLVM (EQU _) = LLVMEQU
            opToLLVM (NE _)  = LLVMNE
            
            getLLVMType :: LLVMVal -> LLVMType
            getLLVMType (IntVal _)  = LLVMInt
            getLLVMType (BoolVal _) = LLVMBool
            getLLVMType (StrVal _)  = LLVMStr
            getLLVMType (RegVal typ _) = typ
    
    compileExpr (EAnd _ expr1 expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (BoolVal val1, BoolVal val2) ->
                return (BoolVal (val1 && val2), instrs1 ++ instrs2)
            (BoolVal False, _) ->
                return (BoolVal False, instrs1)
            (_, BoolVal False) ->
                return (BoolVal False, instrs2)
            (lhs, rhs) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = LLVMBin (LLVMReg nextReg) LLVMAnd LLVMBool lhs rhs
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMBool (LLVMReg nextReg), instrs1 ++ instrs2 ++ [instr])
    
    compileExpr (EOr _ expr1 expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (BoolVal val1, BoolVal val2) ->
                return (BoolVal (val1 || val2), instrs1 ++ instrs2)
            (BoolVal True, _) ->
                return (BoolVal True, instrs1)
            (_, BoolVal True) ->
                return (BoolVal True, instrs2)
            (lhs, rhs) -> do
                (nextLoc, nextReg, env, store) <- get
                let instr = LLVMBin (LLVMReg nextReg) LLVMOr LLVMBool lhs rhs
                put (nextLoc, nextReg + 1, env, store)
                return (RegVal LLVMBool (LLVMReg nextReg), instrs1 ++ instrs2 ++ [instr])
