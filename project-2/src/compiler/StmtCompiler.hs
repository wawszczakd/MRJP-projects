module StmtCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import Data.Maybe
    import ExprCompiler
    import LLVMInstructions
    import UtilsCompiler
    
    compileStmts :: [Stmt] -> CompilerMonad [LLVMInstr]
    compileStmts [] = return []
    compileStmts (stmt:stmts) = do
        instrs <- compileStmt stmt
        case stmt of
            Ret _ _ -> return instrs
            VRet _  -> return instrs
            _       -> do
                restInstrs <- compileStmts stmts
                return $ instrs ++ restInstrs
    
    compileStmt :: Stmt -> CompilerMonad [LLVMInstr]
    
    compileStmt (Empty _) =
        return []
    
    compileStmt (BStmt _ (Blck _ stmts)) = do
        state <- get
        instrs <- compileStmts stmts
        newState <- get
        put newState { funEnv = funEnv state, varEnv = varEnv state }
        return instrs
    
    compileStmt (Decl _ typ vars) = do
        concat <$> mapM (insertVar typ) vars
        where
            insertVar :: Type -> Item -> CompilerMonad [LLVMInstr]
            insertVar typ (NoInit _ name) = do
                state <- get
                let newVarEnv = Data.Map.insert name (nextLoc state) (varEnv state)
                    newStore = case typ of
                        (Int _)  -> Data.Map.insert (nextLoc state) (IntVal 0) (store state)
                        (Bool _) -> Data.Map.insert (nextLoc state) (BoolVal False) (store state)
                        (Str _)  -> Data.Map.insert (nextLoc state) (StrVal "") (store state)
                put state { nextLoc = nextLoc state + 1, varEnv = newVarEnv, store = newStore }
                return []
            insertVar typ (Init _ name expr) = do
                (val, instrs) <- compileExpr expr
                state <- get
                let newVarEnv = Data.Map.insert name (nextLoc state) (varEnv state)
                    newStore = Data.Map.insert (nextLoc state) val (store state)
                put state { nextLoc = nextLoc state + 1, varEnv = newVarEnv, store = newStore }
                return instrs
    
    compileStmt (Ass _ (LVar _ name) expr) = do
        (val, instrs) <- compileExpr expr
        state <- get
        let Just loc = Data.Map.lookup name (varEnv state)
            newStore = Data.Map.insert loc val (store state)
        put state { store = newStore }
        return instrs
    
    compileStmt (Incr _ (LVar _ name)) = do
        state <- get
        let Just loc = Data.Map.lookup name (varEnv state)
            Just val = Data.Map.lookup loc (store state)
        case val of
            (IntVal n) -> do
                let newStore = Data.Map.insert loc (IntVal (n + 1)) (store state)
                put state { store = newStore }
                return []
            (RegVal typ reg) -> do
                let newStore = Data.Map.insert loc (RegVal LLVMInt (LLVMReg (nextReg state))) (store state)
                put state { nextReg = nextReg state + 1, store = newStore }
                return [LLVMBin (LLVMReg (nextReg state)) LLVMPlus LLVMInt (RegVal LLVMInt reg) (IntVal 1)]
    
    compileStmt (Decr _ (LVar _ name)) = do
        state <- get
        let Just loc = Data.Map.lookup name (varEnv state)
            Just val = Data.Map.lookup loc (store state)
        case val of
            (IntVal n) -> do
                let newStore = Data.Map.insert loc (IntVal (n - 1)) (store state)
                put state { store = newStore }
                return []
            (RegVal typ reg) -> do
                let newStore = Data.Map.insert loc (RegVal LLVMInt (LLVMReg (nextReg state))) (store state)
                put state { nextReg = nextReg state + 1, store = newStore }
                return [LLVMBin (LLVMReg (nextReg state)) LLVMMinus LLVMInt (RegVal LLVMInt reg) (IntVal 1)]
    
    compileStmt (Ret _ expr) = do
        (val, instrs) <- compileExpr expr
        return $ instrs ++ [LLVMRet (toLLVMValT val)]
    
    compileStmt (VRet _) =
        return [LLVMRetVoid]
    
    compileStmt (Cond _ expr stmt) = do
        (val, instrs) <- compileExpr expr
        case val of
            (BoolVal True) -> do
                instrs1 <- compileStmt stmt
                return $ instrs ++ instrs1
            (BoolVal False) ->
                return instrs
            (RegVal _ (LLVMReg reg)) -> do
                state <- get
                
                put state { nextLabel = nextLabel state + 1 }
                instrs1 <- compileStmt stmt
                
                state1 <- get
                
                let instrs1Label = LLVMLabel (LLVMLab (nextLabel state)) : instrs1 ++ [LLVMBr (LLVMLab (nextLabel state1))]
                    brInstr      = LLVMBrCond (RegVal LLVMBool (LLVMReg reg)) (LLVMLab (nextLabel state)) (LLVMLab (nextLabel state1))
                    endLabel     = LLVMLabel (LLVMLab (nextLabel state1))
                
                let differingVars = [ loc | (var, loc) <- Data.Map.toList (varEnv state)
                                          , let Just val = Data.Map.lookup loc (store state)
                                          , let Just val1 = Data.Map.lookup loc (store state1)
                                          , val /= val1 ]
                let phiInstrs = [ LLVMPhi (LLVMReg (nextReg state1 + fromIntegral i)) LLVMInt
                                  [(fromJust (Data.Map.lookup loc (store state)), LLVMLab (nextLabel state - 1))
                                  ,(fromJust (Data.Map.lookup loc (store state1)), LLVMLab (nextLabel state1 - 1))]
                                | (i, loc) <- zip [0..] differingVars ]
                    newStore = Data.List.foldl (\acc (i, loc) -> Data.Map.insert loc (RegVal LLVMInt (LLVMReg (nextReg state1 + fromIntegral i))) acc)
                                                (store state)
                                                (zip [0..] differingVars)
                
                let nextReg1' = nextReg state1 + fromIntegral (length phiInstrs)
                put state1 { nextReg = nextReg1', nextLabel = nextLabel state1 + 1, funEnv = funEnv state, varEnv = varEnv state, store = newStore }
                
                return $ instrs ++ [brInstr] ++ instrs1Label ++ [endLabel] ++ phiInstrs
    
    compileStmt (CondElse _ expr stmt1 stmt2) = do
        (val, instrs) <- compileExpr expr
        case val of
            (BoolVal True) -> do
                instrs1 <- compileStmt stmt1
                return $ instrs ++ instrs1
            (BoolVal False) -> do
                instrs2 <- compileStmt stmt2
                return $ instrs ++ instrs2
            (RegVal _ (LLVMReg reg)) -> do
                state <- get
                put state {nextLabel = nextLabel state + 1}
                instrs1 <- compileStmt stmt1
                
                state1 <- get
                put state1 {nextLabel = nextLabel state1 + 1, funEnv = funEnv state, varEnv = varEnv state, store = store state}
                instrs2 <- compileStmt stmt2
                
                state2 <- get
                
                let instrs1Label = LLVMLabel (LLVMLab (nextLabel state)) : instrs1 ++ [LLVMBr (LLVMLab (nextLabel state2))]
                    instrs2Label = LLVMLabel (LLVMLab (nextLabel state1)) : instrs2 ++ [LLVMBr (LLVMLab (nextLabel state2))]
                    brInstr      = LLVMBrCond (RegVal LLVMBool (LLVMReg reg)) (LLVMLab (nextLabel state)) (LLVMLab (nextLabel state1))
                    endLabel     = LLVMLabel (LLVMLab (nextLabel state2))
                
                let differingVars = [ loc | (var, loc) <- Data.Map.toList (varEnv state)
                                          , let Just val = Data.Map.lookup loc (store state)
                                          , let Just val1 = Data.Map.lookup loc (store state1)
                                          , let Just val2 = Data.Map.lookup loc (store state2)
                                          , val /= val1 || val /= val2 ]
                let phiInstrs = [ LLVMPhi (LLVMReg (nextReg state2 + fromIntegral i)) LLVMInt
                                  [(fromJust (Data.Map.lookup loc (store state1)), LLVMLab (nextLabel state1 - 1))
                                  ,(fromJust (Data.Map.lookup loc (store state2)), LLVMLab (nextLabel state2 - 1))]
                                | (i, loc) <- zip [0..] differingVars ]
                    newStore = Data.List.foldl (\acc (i, loc) -> Data.Map.insert loc (RegVal LLVMInt (LLVMReg (nextReg state2 + fromIntegral i))) acc)
                                                (store state)
                                                (zip [0..] differingVars)
                
                let nextReg2' = nextReg state2 + fromIntegral (length phiInstrs)
                put state2 { nextReg = nextReg2', nextLabel = nextLabel state2 + 1, funEnv = funEnv state, varEnv = varEnv state, store = newStore }
                
                return $ instrs ++ [brInstr] ++ instrs1Label ++ instrs2Label ++ [endLabel] ++ phiInstrs
    
    compileStmt (While _ expr stmt) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (SExp _ expr) = do
        (_, instrs) <- compileExpr expr
        return instrs
