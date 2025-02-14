module Compiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import LLVMInstructions
    import StmtCompiler
    import UtilsCompiler
    
    compileProgram :: Program -> IO [LLVMInstr]
    compileProgram (Program _ topDefs) = do
        (_, stateWithFuncs) <- runStateT (insertFuncs topDefs) CompilerState {
            nextLoc    = 0,
            nextReg    = 0,
            nextLabel  = 1,
            funEnv     = Data.Map.empty,
            varEnv     = Data.Map.empty,
            store      = Data.Map.empty,
            strDec     = Data.Map.empty,
            strLoadReg = Data.Map.empty,
            nextStr    = 0 }
        (programBody, finalState) <- runStateT (compileTopDefs topDefs) stateWithFuncs
        let programHead = [ LLVMFunDec LLVMVoid "printInt" [LLVMArgDec LLVMInt]
                          , LLVMFunDec LLVMVoid "printString" [LLVMArgDec LLVMStr]
                          , LLVMFunDec LLVMVoid "error" []
                          , LLVMFunDec LLVMInt "readInt" []
                          , LLVMFunDec LLVMStr "readString" []
                          , LLVMFunDec LLVMBool "__equString" [LLVMArgDec LLVMStr, LLVMArgDec LLVMStr]
                          , LLVMFunDec LLVMBool "__neString" [LLVMArgDec LLVMStr, LLVMArgDec LLVMStr]
                          , LLVMFunDec LLVMStr "__concatString" [LLVMArgDec LLVMStr, LLVMArgDec LLVMStr]
                          ]
            strDecs = [ LLVMStrDec (LLVMString s n) | (s, n) <- sortBy (\(_, n1) (_, n2) -> compare n1 n2) (Data.Map.toList (strDec finalState)) ]
        case strDecs of
            [] -> return $ programHead ++ programBody
            _ ->  return $ programHead ++ [LLVMEmpty] ++ strDecs ++ programBody
    
    insertFuncs :: [TopDef] -> CompilerMonad ()
    insertFuncs topDefs = do
        let builtInFuncs = [ ("printInt", LLVMVoid)
                           , ("printString", LLVMVoid)
                           , ("error", LLVMVoid)
                           , ("readInt", LLVMInt)
                           , ("readString", LLVMStr) ]
        modify (\state ->
            state { funEnv = Data.List.foldr (\(name, entry) acc -> Data.Map.insert (Ident name) entry acc) (funEnv state)builtInFuncs })
        foldM_ (\_ (FnDef _ typ name _ _) -> do
                let funName = getFunName name
                    retType = typeToLLVM typ
                modify (\state ->
                    state { funEnv = Data.Map.insert (Ident funName) retType (funEnv state) })
                return ()) () topDefs
    
    compileTopDefs :: [TopDef] -> CompilerMonad [LLVMInstr]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [LLVMEmpty] ++ topDefCode) [] topDefs
    
    compileTopDef :: TopDef -> CompilerMonad [LLVMInstr]
    compileTopDef (FnDef _ typ name args (Block _ stmts)) = do
        state <- get
        
        mapM_ insertArg args
        instrs <- compileStmts stmts
        let funName = getFunName name
            funBody = LLVMLabel (LLVMLab 0) : instrs
            retType = typeToLLVM typ
            llvmArgs = zipWith (formatArg (nextReg state)) [0..] args
            lastInstr = if Data.List.null funBody then Nothing else Just (last funBody)
            funDef = case lastInstr of
                        Just (LLVMRet _) -> LLVMFunDef retType funName llvmArgs funBody
                        Just LLVMRetVoid -> LLVMFunDef retType funName llvmArgs funBody
                        _ -> LLVMFunDef retType funName llvmArgs (funBody ++ [addReturn typ])
        
        newState <- get
        put state { strDec = strDec newState, nextStr = nextStr newState }
        return [funDef]
        where
            insertArg :: Arg -> CompilerMonad ()
            insertArg (Arg _ typ name) = do
                state <- get
                let newVarEnv = Data.Map.insert name (nextLoc state) (varEnv state)
                    newStore = Data.Map.insert (nextLoc state) (RegVal (typeToLLVM typ) (LLVMReg (nextReg state))) (store state)
                put state { nextLoc = nextLoc state + 1, nextReg = nextReg state + 1, varEnv = newVarEnv, store = newStore }
            
            formatArg :: Integer -> Integer -> Arg -> LLVMArg
            formatArg curNextReg index (Arg _ typ (Ident name)) =
                LLVMArg (typeToLLVM typ) (LLVMReg (curNextReg + index))
            
            addReturn :: Type -> LLVMInstr
            addReturn (Void _) = LLVMRetVoid
            addReturn (Int _) = LLVMRet (toLLVMValT (IntVal 0))
            addReturn (Bool _) = LLVMRet (toLLVMValT (BoolVal False))
