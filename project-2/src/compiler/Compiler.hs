module Compiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import StmtCompiler
    import UtilsCompiler
    
    compileProgram :: Program -> String
    compileProgram (Prog _ topDefs) =
        let
            (_, envWithFuncs) = runState (insertFuncs topDefs) (0, Data.Map.empty)
            (programBody, _) = runState (compileTopDefs topDefs) envWithFuncs
            programHead = [ "declare i32 @readInt()"
                          , "declare i8* @readString()"
                          , "declare void @printInt(i32)"
                          , "declare void @printString(i8*)" ]
        in
            unlines (programHead ++ programBody)
    
    insertFuncs :: [TopDef] -> CompilerMonad ()
    insertFuncs topDefs = do
        let builtInFuncs = [ ("readInt", FuncEntry "i32")
                           , ("readString", FuncEntry "i8*")
                           , ("printInt", FuncEntry "void")
                           , ("printString", FuncEntry "void") ]
        modify (\(nextReg, env) -> 
            (nextReg, Data.List.foldr (\(name, entry) -> Data.Map.insert (Ident name) entry) env builtInFuncs))
        foldM_ (\_ topDef -> do
                case topDef of
                    TopFunDef _ (FnDef _ typ name _ _) -> do
                        let retType = compileType typ
                        modify (\(nextReg, env) -> 
                            (nextReg, Data.Map.insert name (FuncEntry retType) env))
                    _ -> return ()
                return ()) () topDefs
    
    compileTopDefs :: [TopDef] -> CompilerMonad [String]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [""] ++ topDefCode) [] topDefs
    
    compileTopDef :: TopDef -> CompilerMonad [String]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args block)) = do
        let
            retType = compileType typ
            llvmArgs = intercalate ", " (Data.List.map compileArg args)
            funcHeader = "define " ++ retType ++ " @" ++ name ++ "(" ++ llvmArgs ++ ") {"
            funcFooter = "}"
        funcBody <- compileBlock block
        return $ [funcHeader] ++ funcBody ++ [funcFooter]
    
    compileBlock :: Block -> CompilerMonad [String]
    compileBlock (Blck _ stmts) = do
        env <- get
        instrs <- compileStmts stmts
        put env
        return instrs
