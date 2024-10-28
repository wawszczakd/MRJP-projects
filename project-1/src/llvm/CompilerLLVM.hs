module CompilerLLVM where
    import Data.List
    import Data.Map
    import Control.Monad.State
    import AbsInstant
    
    -- (next available register, map of variables to a pair (allocated register, loaded register)) --
    type StateMonad = State (Integer, Data.Map.Map String (Integer, Integer))
    
    data ExprVal = Num Integer | Reg Integer
    
    regToString :: Integer -> String
    regToString reg =
        "%" ++ show reg
    
    exprValToString :: ExprVal -> String
    exprValToString (Num num) =
        show num
    exprValToString (Reg reg) =
        regToString reg
    
    doArithmeticOp :: Exp -> Exp -> String -> StateMonad (ExprVal, [String])
    doArithmeticOp expr1 expr2 op = do
        (exprVal1, code1) <- compileExpr expr1
        (exprVal2, code2) <- compileExpr expr2
        (lastReg, varMap) <- get
        put (lastReg + 1, varMap)
        let
            resReg = regToString lastReg
            arg1 = exprValToString exprVal1
            arg2 = exprValToString exprVal2
            instr = resReg ++ " = " ++ op ++ " i32 " ++ arg1 ++ ", " ++ arg2
        return (Reg lastReg, code1 ++ code2 ++ [instr])
    
    -- (expression value (number or register), generated LLVM code) --
    compileExpr :: Exp -> StateMonad (ExprVal, [String])
    compileExpr (ExpAdd expr1 expr2) = do
        doArithmeticOp expr1 expr2 "add"
    compileExpr (ExpSub expr1 expr2) = do
        doArithmeticOp expr1 expr2 "sub"
    compileExpr (ExpMul expr1 expr2) = do
        doArithmeticOp expr1 expr2 "mul"
    compileExpr (ExpDiv expr1 expr2) = do
        doArithmeticOp expr1 expr2 "sdiv"
    compileExpr (ExpLit num) =
        return (Num num, [])
    compileExpr (ExpVar (Ident var)) = do
        (_, varMap) <- get
        let Just (_, loadReg) = Data.Map.lookup var varMap
        return (Reg loadReg, [])
    
    compileStmt :: Stmt -> StateMonad [String]
    compileStmt (SAss (Ident var) expr) = do
        (lastReg, varMap) <- get
        (exprVal, code) <- compileExpr expr
        case Data.Map.lookup var varMap of
            Just (allocReg, _) -> do
                put (lastReg + 1, Data.Map.insert var (allocReg, lastReg) varMap)
                let
                    storeInstr = "store i32 " ++ exprValToString exprVal ++ ", i32* " ++ regToString allocReg
                    loadInstr = regToString lastReg ++ " = load i32, i32* " ++ regToString allocReg
                return (code ++ [storeInstr, loadInstr])
            Nothing -> do
                put (lastReg + 2, Data.Map.insert var (lastReg, lastReg + 1) varMap)
                let
                    allocInstr = regToString lastReg ++ " = alloca i32"
                    storeInstr = "store i32 " ++ exprValToString exprVal ++ ", i32* " ++ regToString lastReg
                    loadInstr = regToString (lastReg + 1) ++ " = load i32, i32* " ++ regToString lastReg
                return (code ++ [allocInstr, storeInstr, loadInstr])
    
    compileStmt (SExp expr) = do
        (exprVal, code) <- compileExpr expr
        let printCall = "call void @printInt(i32 " ++ exprValToString exprVal ++ ")"
        return (code ++ [printCall])
    
    compileStmts :: [Stmt] -> StateMonad [String]
    compileStmts stmts = do
        foldM (\acc stmt -> do
                    stmtCode <- compileStmt stmt
                    return $ acc ++ stmtCode ++ [""]) [] stmts
    
    compileProgramLLVM :: Program -> String
    compileProgramLLVM (Prog stmts) =
        let
            (programBody, _) = runState (compileStmts stmts) (0, empty)
            programBodyIndented = Data.List.map ("    " ++) programBody
            programHead = [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
                          , ""
                          , "declare i32 @printf(i8*, ...)"
                          , ""
                          , "define void @printInt(i32 %x) {"
                          , "    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
                          , "    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
                          , "    ret void"
                          , "}"
                          , ""
                          , "define i32 @main() {"
                          , "entry:" ]
            programTail = [ "    ret i32 0"
                          , "}" ]
        in
            unlines (programHead ++ programBodyIndented ++ programTail)
