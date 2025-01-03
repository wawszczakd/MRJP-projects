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
        (_, _, _, env, _) <- get
        instrs <- compileStmts stmts
        (nextLoc, nextReg, nextLabel, _, store) <- get
        put (nextLoc, nextReg, nextLabel, env, store)
        return instrs
    
    compileStmt (Decl _ typ vars) = do
        concat <$> mapM (insertVar typ) vars
        where
            insertVar :: Type -> Item -> CompilerMonad [LLVMInstr]
            insertVar typ (NoInit _ name) = do
                (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
                let newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = case typ of
                        (Int _)  -> Data.Map.insert nextLoc (IntVal 0) store
                        (Bool _) -> Data.Map.insert nextLoc (BoolVal False) store
                        (Str _)  -> Data.Map.insert nextLoc (StrVal "") store
                put (nextLoc + 1, nextReg, nextLabel, (funEnv, newVarEnv), newStore)
                return []
            insertVar typ (Init _ name expr) = do
                (val, instrs) <- compileExpr expr
                (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
                let newVarEnv = Data.Map.insert name nextLoc varEnv
                    newStore = Data.Map.insert nextLoc val store
                put (nextLoc + 1, nextReg, nextLabel, (funEnv, newVarEnv), newStore)
                return instrs
    
    compileStmt (Ass _ (LVar _ name) expr) = do
        (val, instrs) <- compileExpr expr
        (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
        let Just loc = Data.Map.lookup name varEnv
            newStore = Data.Map.insert loc val store
        put (nextLoc, nextReg, nextLabel, (funEnv, varEnv), newStore)
        return instrs
    
    compileStmt (Incr _ (LVar _ name)) = do
        (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
        let Just loc = Data.Map.lookup name varEnv
            Just val = Data.Map.lookup loc store
        case val of
            (IntVal n) -> do
                let newStore = Data.Map.insert loc (IntVal (n + 1)) store
                put (nextLoc, nextReg, nextLabel, (funEnv, varEnv), newStore)
                return []
            (RegVal typ reg) -> do
                let newStore = Data.Map.insert loc (RegVal LLVMInt (LLVMReg nextReg)) store
                put (nextLoc, nextReg + 1, nextLabel, (funEnv, varEnv), newStore)
                return [LLVMBin (LLVMReg nextReg) LLVMPlus LLVMInt (RegVal LLVMInt reg) (IntVal 1)]
    
    compileStmt (Decr _ (LVar _ name)) = do
        (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
        let Just loc = Data.Map.lookup name varEnv
            Just val = Data.Map.lookup loc store
        case val of
            (IntVal n) -> do
                let newStore = Data.Map.insert loc (IntVal (n - 1)) store
                put (nextLoc, nextReg, nextLabel, (funEnv, varEnv), newStore)
                return []
            (RegVal typ reg) -> do
                let newStore = Data.Map.insert loc (RegVal LLVMInt (LLVMReg nextReg)) store
                put (nextLoc, nextReg + 1, nextLabel, (funEnv, varEnv), newStore)
                return [LLVMBin (LLVMReg nextReg) LLVMMinus LLVMInt (RegVal LLVMInt reg) (IntVal 1)]
    
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
                (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
                
                put (nextLoc, nextReg, nextLabel + 1, (funEnv, varEnv), store)
                instrs1 <- compileStmt stmt
                
                (nextLoc1, nextReg1, nextLabel1, _, store1) <- get
                
                let instrs1Label = LLVMLabel (LLVMLab nextLabel) : instrs1 ++ [LLVMBr (LLVMLab nextLabel1)]
                    brInstr      = LLVMBrCond (RegVal LLVMBool (LLVMReg reg)) (LLVMLab nextLabel) (LLVMLab nextLabel1)
                    endLabel     = LLVMLabel (LLVMLab nextLabel1)
                
                let differingVars = [ loc | (var, loc) <- Data.Map.toList varEnv
                                          , let Just val = Data.Map.lookup loc store
                                          , let Just val1 = Data.Map.lookup loc store1
                                          , val /= val1 ]
                let phiInstrs = [ LLVMPhi (LLVMReg (nextReg1 + fromIntegral i)) LLVMInt
                                  [(fromJust (Data.Map.lookup loc store), LLVMLab (nextLabel - 1))
                                  ,(fromJust (Data.Map.lookup loc store1), LLVMLab (nextLabel1 - 1))]
                                | (i, loc) <- zip [0..] differingVars ]
                    newStore = Data.List.foldl (\acc (i, loc) -> Data.Map.insert loc (RegVal LLVMInt (LLVMReg (nextReg1 + fromIntegral i))) acc)
                                                store
                                                (zip [0..] differingVars)
                
                let nextReg1' = nextReg1 + fromIntegral (length phiInstrs)
                put (nextLoc1, nextReg1', nextLabel1 + 1, (funEnv, varEnv), newStore)
                
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
                (nextLoc, nextReg, nextLabel, (funEnv, varEnv), store) <- get
                
                put (nextLoc, nextReg, nextLabel + 1, (funEnv, varEnv), store)
                instrs1 <- compileStmt stmt1
                
                (nextLoc1, nextReg1, nextLabel1, _, store1) <- get
                put (nextLoc1, nextReg1, nextLabel1 + 1, (funEnv, varEnv), store)
                instrs2 <- compileStmt stmt2
                
                (nextLoc2, nextReg2, nextLabel2, _, store2) <- get
                
                let instrs1Label = LLVMLabel (LLVMLab nextLabel) : instrs1 ++ [LLVMBr (LLVMLab nextLabel2)]
                    instrs2Label = LLVMLabel (LLVMLab nextLabel1) : instrs2 ++ [LLVMBr (LLVMLab nextLabel2)]
                    brInstr      = LLVMBrCond (RegVal LLVMBool (LLVMReg reg)) (LLVMLab nextLabel) (LLVMLab nextLabel1)
                    endLabel     = LLVMLabel (LLVMLab nextLabel2)
                
                let differingVars = [ loc | (var, loc) <- Data.Map.toList varEnv
                                          , let Just val = Data.Map.lookup loc store
                                          , let Just val1 = Data.Map.lookup loc store1
                                          , let Just val2 = Data.Map.lookup loc store2
                                          , val /= val1 || val /= val2 ]
                let phiInstrs = [ LLVMPhi (LLVMReg (nextReg2 + fromIntegral i)) LLVMInt
                                  [(fromJust (Data.Map.lookup loc store1), LLVMLab (nextLabel1 - 1))
                                  ,(fromJust (Data.Map.lookup loc store2), LLVMLab (nextLabel2 - 1))]
                                | (i, loc) <- zip [0..] differingVars ]
                    newStore = Data.List.foldl (\acc (i, loc) -> Data.Map.insert loc (RegVal LLVMInt (LLVMReg (nextReg2 + fromIntegral i))) acc)
                                                store
                                                (zip [0..] differingVars)
                
                let nextReg2' = nextReg2 + fromIntegral (length phiInstrs)
                put (nextLoc2, nextReg2', nextLabel2 + 1, (funEnv, varEnv), newStore)
                
                return $ instrs ++ [brInstr] ++ instrs1Label ++ instrs2Label ++ [endLabel] ++ phiInstrs
    
    compileStmt (While _ expr stmt) =
        return [LLVMEmpty] -- TODO
    
    compileStmt (SExp _ expr) = do
        (_, instrs) <- compileExpr expr
        return instrs
