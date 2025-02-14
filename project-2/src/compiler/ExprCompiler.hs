module ExprCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import LLVMInstructions
    import UtilsCompiler
    
    compileExpr :: Expr -> CompilerMonad (LLVMVal, [LLVMInstr])
    
    compileExpr (EVar _ name) = do
        state <- get
        let Just loc = Data.Map.lookup name (varEnv state)
        let Just val = Data.Map.lookup loc (store state)
        return (val, [])
    
    compileExpr (ELitInt _ val) =
        return (IntVal val, [])
    
    compileExpr (ELitTrue _) =
        return (BoolVal True, [])
    
    compileExpr (ELitFalse _) =
        return (BoolVal False, [])
    
    compileExpr (EApp _ name args) = do
        state <- get
        let funName = getFunName name
            Just retType = Data.Map.lookup (Ident funName) (funEnv state)
        (argVals, instrs) <- compileArgs args
        newState <- get
        let instr
                | retType == LLVMVoid = LLVMCallVoid funName argVals
                | otherwise = LLVMCall (LLVMReg (nextReg newState)) retType funName argVals
        
        if retType /= LLVMVoid then
            put newState { nextReg = nextReg newState + 1, funEnv = funEnv state, varEnv = varEnv state }
        else
            put newState { funEnv = funEnv state, varEnv = varEnv state }
        
        return (if retType == LLVMVoid then RegVal LLVMVoid (LLVMReg (-1)) else RegVal retType (LLVMReg (nextReg newState)), instrs ++ [instr])
        where
            compileArgs :: [Expr] -> CompilerMonad ([LLVMValT], [LLVMInstr])
            compileArgs [] = return ([], [])
            compileArgs (arg : rest) = do
                (argVal, argCode) <- compileExpr arg
                (fixedArgVal, loadCode) <- fixStringVal argVal
                (restVals, restCodes) <- compileArgs rest
                return (toLLVMValT fixedArgVal : restVals, argCode ++ loadCode ++ restCodes)
    
    compileExpr (EString _ s) = do
        return (StrVal s, [])
    
    compileExpr (Neg _ expr) = do
        (exprVal, instrs) <- compileExpr expr
        case exprVal of
            IntVal val -> return (IntVal (-val), instrs)
            RegVal _ (LLVMReg reg) -> do
                state <- get
                let instr = LLVMBin (LLVMReg (nextReg state)) LLVMMinus LLVMInt (IntVal 0) (RegVal LLVMInt (LLVMReg reg))
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMInt (LLVMReg (nextReg state)), instrs ++ [instr])
    
    compileExpr (Not _ expr) = do
        (exprVal, instrs) <- compileExpr expr
        case exprVal of
            BoolVal val -> return (BoolVal (not val), instrs)
            RegVal LLVMBool (LLVMReg reg) -> do
                state <- get
                let notInstr = LLVMBin (LLVMReg (nextReg state)) LLVMXor LLVMBool (BoolVal True) (RegVal LLVMBool (LLVMReg reg))
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs ++ [notInstr])
    
    compileExpr (EMul _ expr1 op expr2) = do
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (IntVal val1, IntVal val2) ->
                case op of
                    Times _ -> return (IntVal (val1 * val2), instrs1 ++ instrs2)
                    Div _   -> return (IntVal (val1 `div` val2), instrs1 ++ instrs2)
                    Mod _   -> return (IntVal (val1 `rem` val2), instrs1 ++ instrs2)
            (lhs, rhs) -> do
                state <- get
                let instr = LLVMBin (LLVMReg (nextReg state)) (opToLLVM op) LLVMInt lhs rhs
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMInt (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [instr])
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
            (StrVal s1, StrVal s2) ->
                return (StrVal (s1 ++ s2), instrs1 ++ instrs2)
            (StrVal s1, RegVal _ reg2) -> do
                (reg1, regInstrs) <- getStrReg s1
                state <- get
                let callInstr = LLVMCall (LLVMReg (nextReg state)) LLVMStr "__concatString" [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMStr (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ regInstrs ++ [callInstr])
            (RegVal _ reg1, StrVal s2) -> do
                (reg2, regInstrs) <- getStrReg s2
                state <- get
                let callInstr = LLVMCall (LLVMReg (nextReg state)) LLVMStr "__concatString" [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMStr (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ regInstrs ++ [callInstr])
            (RegVal LLVMStr reg1, RegVal LLVMStr reg2) -> do
                state <- get
                let callInstr = LLVMCall (LLVMReg (nextReg state)) LLVMStr "__concatString" [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMStr (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [callInstr])
            (lhs, rhs) -> do
                state <- get
                let instr = LLVMBin (LLVMReg (nextReg state)) (opToLLVM op) LLVMInt lhs rhs
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMInt (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [instr])
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
            (StrVal s1, RegVal _ reg2) -> do
                (reg1, regInstrs) <- getStrReg s1
                state <- get
                let callInstr = case op of
                        EQU _ -> LLVMCall (LLVMReg (nextReg state)) LLVMBool "__equString"
                                 [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                        NE _  -> LLVMCall (LLVMReg (nextReg state)) LLVMBool "__neqString"
                                 [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ regInstrs ++ [callInstr])
            (RegVal _ reg1, StrVal s2) -> do
                (reg2, regInstrs) <- getStrReg s2
                state <- get
                let callInstr = case op of
                        EQU _ -> LLVMCall (LLVMReg (nextReg state)) LLVMBool "__equString"
                                 [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                        NE _  -> LLVMCall (LLVMReg (nextReg state)) LLVMBool "__neqString"
                                 [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ regInstrs ++ [callInstr])
            (RegVal LLVMStr reg1, RegVal LLVMStr reg2) -> do
                state <- get
                let callInstr = case op of
                        EQU _ -> LLVMCall (LLVMReg (nextReg state)) LLVMBool "__equString"
                                 [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                        NE _  -> LLVMCall (LLVMReg (nextReg state)) LLVMBool "__neqString"
                                 [toLLVMValT (RegVal LLVMStr reg1), toLLVMValT (RegVal LLVMStr reg2)]
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [callInstr])
            (RegVal typ1 (LLVMReg reg1), RegVal typ2 (LLVMReg reg2)) ->
                if reg1 == reg2 then
                    let result = case op of
                            LTH _ -> False
                            LE _  -> True
                            GTH _ -> False
                            GE _  -> True
                            EQU _ -> True
                            NE _  -> False
                    in return (BoolVal result, instrs1 ++ instrs2)
                else do
                    state <- get
                    let instr = LLVMBin (LLVMReg (nextReg state)) (opToLLVM op) typ1 (RegVal typ1 (LLVMReg reg1)) (RegVal typ2 (LLVMReg reg2))
                    put state { nextReg = nextReg state + 1 }
                    return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [instr])
            (lhs, rhs) -> do
                state <- get
                let instr = LLVMBin (LLVMReg (nextReg state)) (opToLLVM op) (getLLVMType lhs) lhs rhs
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [instr])
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
        iniState <- get
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (BoolVal val1, BoolVal val2) ->
                return (BoolVal (val1 && val2), instrs1 ++ instrs2)
            (BoolVal False, _) ->
                return (BoolVal False, instrs1)
            (_, BoolVal False) ->
                return (BoolVal False, instrs2)
            (RegVal _ _, RegVal _ _) -> do
                put iniState
                (reg1, instrs1) <- compileExpr expr1
                state1 <- get
                put state1 { nextLabel = nextLabel state1 + 1 }
                (reg2, instrs2) <- compileExpr expr2
                state2 <- get
                let labTrue = LLVMLab (nextLabel state1)
                    labFalse = LLVMLab (nextLabel state2)
                    labFinal = LLVMLab (nextLabel state2 + 1)
                    brInstr1 = LLVMBrCond reg1 labTrue labFalse
                    brInstr2 = LLVMBrCond reg2 labFinal labFalse
                    phiInstr = LLVMPhi (LLVMReg (nextReg state2)) LLVMBool [(BoolVal True, LLVMLab (nextLabel state2 - 1)), (BoolVal False, labFalse)]
                    allInstrs = instrs1 ++ [brInstr1] ++ [LLVMLabel labTrue] ++ instrs2 ++ [brInstr2] ++ [LLVMLabel labFalse]
                                ++ [LLVMBr labFinal] ++ [LLVMLabel labFinal] ++ [phiInstr]
                put state2 { nextReg = nextReg state2 + 1, nextLabel = nextLabel state2 + 2 }
                return (RegVal LLVMBool (LLVMReg (nextReg state2)), allInstrs)
            (lhs, rhs) -> do
                state <- get
                let instr = LLVMBin (LLVMReg (nextReg state)) LLVMAnd LLVMBool lhs rhs
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [instr])
    
    compileExpr (EOr _ expr1 expr2) = do
        iniState <- get
        (exprVal1, instrs1) <- compileExpr expr1
        (exprVal2, instrs2) <- compileExpr expr2
        case (exprVal1, exprVal2) of
            (BoolVal val1, BoolVal val2) ->
                return (BoolVal (val1 || val2), instrs1 ++ instrs2)
            (BoolVal True, _) ->
                return (BoolVal True, instrs1)
            (_, BoolVal True) ->
                return (BoolVal True, instrs2)
            (RegVal _ _, RegVal _ _) -> do
                put iniState
                (reg1, instrs1) <- compileExpr expr1
                state1 <- get
                put state1 { nextLabel = nextLabel state1 + 1 }
                (reg2, instrs2) <- compileExpr expr2
                state2 <- get
                let labFalse = LLVMLab (nextLabel state1)
                    labTrue = LLVMLab (nextLabel state2)
                    labFinal = LLVMLab (nextLabel state2 + 1)
                    brInstr1 = LLVMBrCond reg1 labTrue labFalse
                    brInstr2 = LLVMBrCond reg2 labTrue labFinal
                    phiInstr = LLVMPhi (LLVMReg (nextReg state2)) LLVMBool [(BoolVal True, labTrue), (BoolVal False, LLVMLab (nextLabel state2 - 1))]
                    allInstrs = instrs1 ++ [brInstr1] ++ [LLVMLabel labFalse] ++ instrs2 ++ [brInstr2] ++ [LLVMLabel labTrue]
                                ++ [LLVMBr labFinal] ++ [LLVMLabel labFinal] ++ [phiInstr]
                put state2 { nextReg = nextReg state2 + 1, nextLabel = nextLabel state2 + 2 }
                return (RegVal LLVMBool (LLVMReg (nextReg state2)), allInstrs)
            (lhs, rhs) -> do
                state <- get
                let instr = LLVMBin (LLVMReg (nextReg state)) LLVMOr LLVMBool lhs rhs
                put state { nextReg = nextReg state + 1 }
                return (RegVal LLVMBool (LLVMReg (nextReg state)), instrs1 ++ instrs2 ++ [instr])
