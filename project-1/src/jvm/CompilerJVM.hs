module CompilerJVM where
    import System.FilePath
    import Data.List
    import Data.List.Split
    import Data.Map
    import Control.Monad.State
    import AbsInstant
    
    type StateMonad = State (Data.Map.Map String Integer)
    
    doArithmeticOp :: Exp -> Exp -> String -> StateMonad(Integer, [String])
    doArithmeticOp expr1 expr2 op = do
        (depth1, code1) <- compileExpr expr1
        (depth2, code2) <- compileExpr expr2
        if depth1 >= depth2 then
            return (depth1 + 1, code1 ++ code2 ++ [op])
        else
            return (depth2 + 1, code2 ++ code1 ++ ["swap", op])
    
    compileExpr :: Exp -> StateMonad (Integer, [String])
    compileExpr (ExpAdd expr1 expr2) = do
        doArithmeticOp expr1 expr2 "iadd"
    compileExpr (ExpSub expr1 expr2) = do
        doArithmeticOp expr1 expr2 "isub"
    compileExpr (ExpMul expr1 expr2) = do
        doArithmeticOp expr1 expr2 "imul"
    compileExpr (ExpDiv expr1 expr2) = do
        doArithmeticOp expr1 expr2 "idiv"
    compileExpr (ExpLit num) =
        if 0 <= num && num <= 5 then
            return (1, ["iconst_" ++ show num])
        else
            return (1, ["ldc " ++ show num])
    
    compileStmt :: Stmt -> StateMonad [String]
    compileStmt (SAss (Ident var) expr) = return [show var, show expr]
    
    compileStmt (SExp expr) = do
        (_, code) <- compileExpr expr
        return (code ++ [ "getstatic java/lang/System/out Ljava/io/PrintStream;"
                        , "swap"
                        , "invokevirtual java/io/PrintStream/println(I)V" ])
    
    compileStmts :: [Stmt] -> StateMonad [String]
    compileStmts stmts = do
        foldM (\acc stmt -> do
                    stmtCode <- compileStmt stmt
                    return $ acc ++ stmtCode ++ [""]) [] stmts
    
    compileProgramJVM :: Program -> String -> String
    compileProgramJVM (Prog stmts) file =
        let
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
                          , ".limit stack 1000" ] -- TODO
            programTail = [ "    return"
                          , ".end method" ]
            (programBody, _) = runState (compileStmts stmts) empty
            programBodyIndented = Data.List.map ("    " ++) programBody
        in
            unlines (programHead ++ programBodyIndented ++ programTail)
