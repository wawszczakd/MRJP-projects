module CompilerLLVM where
    import AbsInstant
    
    compileStmts :: [Stmt] -> String
    compileStmts stmts = show stmts
    
    compileProgram :: Program -> String
    compileProgram (Prog stmts) = compileStmts stmts