module CompilerJVM where
    import System.FilePath
    import Data.List
    import Data.List.Split
    import Data.Map
    import Control.Monad.State
    import AbsInstant
    
    type StateMonad = State (Integer, Data.Map.Map String Integer)
    
    loadInstr :: Integer -> String
    loadInstr loc
        | loc <= 3  = "iload_" ++ show loc
        | otherwise = "iload " ++ show loc
    
    storeInstr :: Integer -> String
    storeInstr loc
        | loc <= 3  = "istore_" ++ show loc
        | otherwise = "istore " ++ show loc
    
    doArithmeticOp :: Exp -> Exp -> String -> StateMonad(Integer, [String])
    doArithmeticOp expr1 expr2 op = do
        (depth1, code1) <- compileExpr expr1
        (depth2, code2) <- compileExpr expr2
        if depth1 >= depth2 then
            return (max depth1 (depth2 + 1), code1 ++ code2 ++ [op])
        else if op == "iadd" || op == "imul" then
            return (max depth2 (depth1 + 1), code2 ++ code1 ++ [op])
        else
            return (max depth2 (depth1 + 1), code2 ++ code1 ++ ["swap", op])
    
    compileExpr :: Exp -> StateMonad (Integer, [String])
    compileExpr (ExpAdd expr1 expr2) = do
        doArithmeticOp expr1 expr2 "iadd"
    compileExpr (ExpSub expr1 expr2) = do
        doArithmeticOp expr1 expr2 "isub"
    compileExpr (ExpMul expr1 expr2) = do
        doArithmeticOp expr1 expr2 "imul"
    compileExpr (ExpDiv expr1 expr2) = do
        doArithmeticOp expr1 expr2 "idiv"
    compileExpr (ExpLit num)
        | 0 <= num && num <= 5 =
            return (1, ["iconst_" ++ show num])
        | -128 <= num && num <= 127 =
            return (1, ["bipush " ++ show num])
        | -65536 <= num && num <= 65535 =
            return (1, ["sipush " ++ show num])
        | otherwise =
            return (1, ["ldc " ++ show num])
    compileExpr (ExpVar (Ident var)) = do
        (_, varMap) <- get
        let Just loc = Data.Map.lookup var varMap
        return (1, [loadInstr loc])
    
    compileStmt :: Stmt -> StateMonad (Integer, [String])
    compileStmt (SAss (Ident var) expr) = do
        (lastLoc, varMap) <- get
        (depth, code) <- compileExpr expr
        case Data.Map.lookup var varMap of
            Just loc -> do
                return (depth, code ++ [storeInstr loc])
            Nothing -> do
                put (lastLoc + 1, Data.Map.insert var lastLoc varMap)
                return (depth, code ++ [storeInstr lastLoc])
    
    compileStmt (SExp expr) = do
        (depth, code) <- compileExpr expr
        return (max depth 2,
                code ++ [ "getstatic java/lang/System/out Ljava/io/PrintStream;"
                        , "swap"
                        , "invokevirtual java/io/PrintStream/println(I)V" ])
    
    compileStmts :: [Stmt] -> StateMonad (Integer, [String])
    compileStmts stmts = do
        foldM (\(maxDepth, code) stmt -> do
                    (depth, stmtCode) <- compileStmt stmt
                    return (max maxDepth depth, code ++ stmtCode ++ [""])) (0, []) stmts
    
    compileProgramJVM :: Program -> String -> String
    compileProgramJVM (Prog stmts) file =
        let
            ((maxDepth, programBody), (lastLoc, _)) = runState (compileStmts stmts) (1, empty)
            programBodyIndented = Data.List.map ("    " ++) programBody
            parts = splitOn "/" file
            baseName = takeWhile (/= '.') (last parts)
            className = intercalate "." (init parts ++ [baseName])
            programHead = [ ".class public " ++ className
                          , ".super java/lang/Object"
                          , ""
                          , "; standard initializer"
                          , ".method public <init>()V"
                          , "    aload_0"
                          , "    invokespecial java/lang/Object/<init>()V"
                          , "    return"
                          , ".end method"
                          , ""
                          , ".method public static main([Ljava/lang/String;)V"
                          , ".limit stack " ++ show maxDepth
                          , ".limit locals " ++ show lastLoc ]
            programTail = [ "    return"
                          , ".end method" ]
        in
            unlines (programHead ++ programBodyIndented ++ programTail)
