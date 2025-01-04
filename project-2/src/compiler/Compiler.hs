module Compiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import LLVMInstructions
    import StmtCompiler
    import UtilsCompiler
    
    compileProgram :: Program -> [LLVMInstr]
    compileProgram (Prog _ topDefs) =
        let
            (_, envWithFuncs) = runState (insertFuncs topDefs) CompilerState {
                nextLoc   = 0,
                nextReg   = 0,
                nextLabel = 1,
                funEnv    = Data.Map.empty,
                varEnv    = Data.Map.empty,
                store     = Data.Map.empty }
            (programBody, _) = runState (compileTopDefs topDefs) envWithFuncs
            programHead = [ LLVMFunDec LLVMVoid "printInt" [LLVMArgDec LLVMInt]
                          , LLVMFunDec LLVMVoid "printString" [LLVMArgDec LLVMStr]
                          , LLVMFunDec LLVMVoid "error" []
                          , LLVMFunDec LLVMInt "readInt" []
                          , LLVMFunDec LLVMStr "readString" [] ]
        in
            programHead ++ programBody
    
    insertFuncs :: [TopDef] -> CompilerMonad ()
    insertFuncs topDefs = do
        let builtInFuncs = [ ("printInt", LLVMVoid)
                           , ("printString", LLVMVoid)
                           , ("error", LLVMVoid)
                           , ("readInt", LLVMInt)
                           , ("readString", LLVMStr) ]
        modify (\state ->
            state { funEnv = Data.List.foldr (\(name, entry) acc -> Data.Map.insert (Ident name) entry acc) (funEnv state)builtInFuncs })
        foldM_ (\_ topDef -> do
                case topDef of
                    TopFunDef _ (FnDef _ typ name _ _) -> do
                        let retType = typeToLLVM typ
                        modify (\state ->
                            state { funEnv = Data.Map.insert name retType (funEnv state) })
                    _ -> return ()
                return ()) () topDefs
    
    compileTopDefs :: [TopDef] -> CompilerMonad [LLVMInstr]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [LLVMEmpty] ++ topDefCode) [] topDefs
    
    compileTopDef :: TopDef -> CompilerMonad [LLVMInstr]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args (Blck _ stmts))) = do
        state <- get
        
        mapM_ insertArg args
        instrs <- compileStmts stmts
        let
            funBody = LLVMLabel (LLVMLab 0) : instrs
            retType = typeToLLVM typ
            llvmArgs = zipWith (formatArg (nextReg state)) [0..] args
            lastInstr = if Data.List.null funBody then Nothing else Just (last funBody)
            funDef = case lastInstr of
                        Just (LLVMRet _) -> LLVMFunDef retType name llvmArgs funBody
                        Just LLVMRetVoid -> LLVMFunDef retType name llvmArgs funBody
                        _ -> LLVMFunDef retType name llvmArgs (funBody ++ [addReturn typ])
        
        put state
        return [funDef]
        where
            insertArg :: Arg -> CompilerMonad ()
            insertArg (Ar _ typ name) = do
                state <- get
                let newVarEnv = Data.Map.insert name (nextLoc state) (varEnv state)
                    newStore = Data.Map.insert (nextLoc state) (RegVal (typeToLLVM typ) (LLVMReg (nextReg state))) (store state)
                put state { nextLoc = nextLoc state + 1, nextReg = nextReg state + 1, varEnv = newVarEnv, store = newStore }
            
            formatArg :: Integer -> Integer -> Arg -> LLVMArg
            formatArg curNextReg index (Ar _ typ (Ident name)) =
                LLVMArg (typeToLLVM typ) (LLVMReg (curNextReg + index))
            
            addReturn :: Type -> LLVMInstr
            addReturn (Void _) = LLVMRetVoid
            addReturn (Int _) = LLVMRet (toLLVMValT (IntVal 0))
            addReturn (Bool _) = LLVMRet (toLLVMValT (BoolVal False))

