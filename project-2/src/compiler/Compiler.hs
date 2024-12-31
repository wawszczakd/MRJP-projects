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
            (_, envWithFuncs) = runState (insertFuncs topDefs) (1, 1, (Data.Map.empty, Data.Map.empty), Data.Map.empty)
            (programBody, _) = runState (compileTopDefs topDefs) envWithFuncs
            programHead = [ "declare i32 @readInt()"
                          , "declare i8* @readString()"
                          , "declare void @printInt(i32)"
                          , "declare void @printString(i8*)" ]
        in
            unlines (programHead ++ programBody)
    
    insertFuncs :: [TopDef] -> CompilerMonad ()
    insertFuncs topDefs = do
        let builtInFuncs = [ ("readInt", "i32")
                           , ("readString", "i8*")
                           , ("printInt", "void")
                           , ("printString", "void") ]
        modify (\(nextLoc, nextReg, (funEnv, varEnv), store) ->
            (nextLoc, nextReg, (Data.List.foldr (\(name, entry) -> Data.Map.insert (Ident name) entry) funEnv builtInFuncs, varEnv), store))
        foldM_ (\_ topDef -> do
                case topDef of
                    TopFunDef _ (FnDef _ typ name _ _) -> do
                        let retType = typeToLLVM typ
                        modify (\(nextLoc, nextReg, (funEnv, varEnv), store) ->
                            (nextLoc, nextReg, (Data.Map.insert name retType funEnv, varEnv), store))
                    _ -> return ()
                return ()) () topDefs
    
    compileTopDefs :: [TopDef] -> CompilerMonad [String]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [""] ++ topDefCode) [] topDefs
    
    compileTopDef :: TopDef -> CompilerMonad [String]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args block)) = do
        (nextLoc, nextReg, env, store) <- get
        
        mapM_ insertArg args
        let
            retType = typeToLLVM typ
            llvmArgs = intercalate ", " $ zipWith (formatArg nextReg) [0..] args
            funcHeader = "define " ++ retType ++ " @" ++ name ++ "(" ++ llvmArgs ++ ") {"
            funcFooter = "}"
        funcBody <- compileBlock block
        
        put (nextLoc, nextReg, env, store)
        return $ [funcHeader] ++ funcBody ++ (addReturn typ) ++ [funcFooter]
        where
            insertArg :: Arg -> CompilerMonad ()
            insertArg (Ar _ _ name) = do
                (nextLoc, nextReg, (funEnv, varEnv), store) <- get
                let
                    newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = Data.Map.insert nextLoc (Re nextReg) store
                put (nextLoc + 1, nextReg + 1, (funEnv, newVarEnv), newStore)
            
            formatArg :: Integer -> Integer -> Arg -> String
            formatArg curNextReg index (Ar _ typ (Ident name)) =
                typeToLLVM typ ++ " %" ++ show (curNextReg + index)
            
            addReturn :: Type -> [String]
            addReturn (Void _) = ["    ret void"]
            addReturn (Int _)  = ["    ret i32 0"]
            addReturn (Bool _) = ["    ret i1 0"]
            addReturn (Str _)  = ["    ret i8* null"]
    
    compileBlock :: Block -> CompilerMonad [String]
    compileBlock (Blck _ stmts) = do
        (_, _, env, _) <- get
        instrs <- compileStmts stmts
        (nextLoc, nextReg, _, store) <- get
        put (nextLoc, nextReg, env, store)
        return instrs
