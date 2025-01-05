module StmtCompiler where
    import AbsLatte
    import Control.Monad.Identity
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
                put state1 { nextLabel = nextLabel state1 + 1, funEnv = funEnv state, varEnv = varEnv state, store = store state, strLoadReg = strLoadReg state }
                
                let differingVars = [ loc | (var, loc) <- Data.Map.toList (varEnv state)
                                          , let Just val = Data.Map.lookup loc (store state)
                                          , let Just val1 = Data.Map.lookup loc (store state1)
                                          , val /= val1 ]
                    
                    trueLabel = LLVMLab (nextLabel state1 - 1)
                    falseLabel = LLVMLab (nextLabel state - 1)
                    
                    getPhiInstr :: CompilerState -> CompilerState -> Integer -> CompilerMonad ([LLVMInstr], [LLVMInstr], [LLVMInstr])
                    getPhiInstr trueState falseState loc = do
                        curState <- get
                        put curState { strLoadReg = strLoadReg trueState }
                        (val1, loadInstrs1) <- fixStringVal (fromJust (Data.Map.lookup loc (store trueState)))
                        
                        curState' <- get
                        put curState' { strLoadReg = strLoadReg falseState }
                        (val2, loadInstrs2) <- fixStringVal (fromJust (Data.Map.lookup loc (store falseState)))
                        
                        state <- get
                        let typ = getTypeFromVal val1
                            reg = nextReg state
                            phiInstr = LLVMPhi (LLVMReg reg) typ [(val1, trueLabel), (val2, falseLabel)]
                        
                        put state { nextReg = reg + 1, store = Data.Map.insert loc (RegVal typ (LLVMReg reg)) (store state), strLoadReg = strLoadReg curState }
                        
                        return (loadInstrs1, loadInstrs2, [phiInstr])
                
                tmp <- mapM (getPhiInstr state1 state) differingVars
                let (loadInstrs1, loadInstrs2, phiInstrs) = unzip3 tmp
                
                let instrs1Label = LLVMLabel (LLVMLab (nextLabel state)) : instrs1 ++ concat loadInstrs1 ++ [LLVMBr (LLVMLab (nextLabel state1))]
                    brInstr      = LLVMBrCond (RegVal LLVMBool (LLVMReg reg)) (LLVMLab (nextLabel state)) (LLVMLab (nextLabel state1))
                    endLabel     = LLVMLabel (LLVMLab (nextLabel state1))
                
                return $ concat loadInstrs2 ++ instrs ++ [brInstr] ++ instrs1Label ++ [endLabel] ++ concat phiInstrs
    
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
                put state { nextLabel = nextLabel state + 1 }
                instrs1 <- compileStmt stmt1
                
                state1 <- get
                put state1 { nextLabel = nextLabel state1 + 1, funEnv = funEnv state, varEnv = varEnv state, store = store state, strLoadReg = strLoadReg state }
                instrs2 <- compileStmt stmt2
                
                state2 <- get
                put state2 { funEnv = funEnv state, varEnv = varEnv state, store = store state, strLoadReg = strLoadReg state }
                
                let differingVars = [ loc | (var, loc) <- Data.Map.toList (varEnv state)
                                          , let Just val = Data.Map.lookup loc (store state)
                                          , let Just val1 = Data.Map.lookup loc (store state1)
                                          , let Just val2 = Data.Map.lookup loc (store state2)
                                          , val /= val1 || val /= val2 ]
                    
                    trueLabel = LLVMLab (nextLabel state1 - 1)
                    falseLabel = LLVMLab (nextLabel state2 - 1)
                    
                    getPhiInstr :: CompilerState -> CompilerState -> Integer -> CompilerMonad ([LLVMInstr], [LLVMInstr], [LLVMInstr])
                    getPhiInstr trueState falseState loc = do
                        curState <- get
                        put curState { strLoadReg = strLoadReg trueState }
                        (val1, loadInstrs1) <- fixStringVal (fromJust (Data.Map.lookup loc (store trueState)))
                        
                        curState' <- get
                        put curState' { strLoadReg = strLoadReg falseState }
                        (val2, loadInstrs2) <- fixStringVal (fromJust (Data.Map.lookup loc (store falseState)))
                        
                        state <- get
                        let typ = getTypeFromVal val1
                            reg = nextReg state
                            phiInstr = LLVMPhi (LLVMReg reg) typ [(val1, trueLabel), (val2, falseLabel)]
                        
                        put state { nextReg = reg + 1, store = Data.Map.insert loc (RegVal typ (LLVMReg reg)) (store state), strLoadReg = strLoadReg curState }
                        
                        return (loadInstrs1, loadInstrs2, [phiInstr])
                    
                tmp <- mapM (getPhiInstr state1 state2) differingVars
                let (loadInstrs1, loadInstrs2, phiInstrs) = unzip3 tmp
                
                let instrs1Label = LLVMLabel (LLVMLab (nextLabel state)) : instrs1 ++ concat loadInstrs1 ++ [LLVMBr (LLVMLab (nextLabel state2))]
                    instrs2Label = LLVMLabel (LLVMLab (nextLabel state1)) : instrs2 ++ concat loadInstrs2 ++ [LLVMBr (LLVMLab (nextLabel state2))]
                    brInstr      = LLVMBrCond (RegVal LLVMBool (LLVMReg reg)) (LLVMLab (nextLabel state)) (LLVMLab (nextLabel state1))
                    endLabel     = LLVMLabel (LLVMLab (nextLabel state2))
                
                return $ instrs ++ [brInstr] ++ instrs1Label ++ instrs2Label ++ [endLabel] ++ concat phiInstrs
    
    compileStmt (While _ expr stmt) = do
        initState <- get
        
        -- liftIO $ print $ "env before: " ++ show (varEnv initState)
        -- liftIO $ print $ "store before: " ++ show (store initState)
        
        let (updatedStore, newNextReg) = Data.Map.foldrWithKey modifyStoreEntry (store initState, nextReg initState) (store initState)
        put initState { store = updatedStore, nextReg = newNextReg }
        
        regStateBeforeStmt <- get
        -- liftIO $ print $ "store changed to regs: " ++ show (store regStateBeforeStmt)
        _ <- compileStmt stmt
        regStateAfterStmt <- get
        -- liftIO $ print $ "store after stmts: " ++ show (store regStateAfterStmt)
        
        let differingVars = [ loc | (var, loc) <- Data.Map.toList (varEnv initState)
                                , let Just val1 = Data.Map.lookup loc (store regStateBeforeStmt)
                                , let Just val2 = Data.Map.lookup loc (store regStateAfterStmt)
                                , val1 /= val2 ]
        
        -- liftIO $ print $ "env after: " ++ show (varEnv regStateAfterStmt)
        -- liftIO $ print $ "differingVars: " ++ show differingVars
        
        put initState
        
        (phiInstrs, loadInstrs) <- foldM generatePhiInstrs ([], []) differingVars
        state <- get
        
        let beginInstrs = [LLVMBr (LLVMLab (nextLabel state)),
                           LLVMLabel (LLVMLab (nextLabel state))]
        put state { nextLabel = nextLabel state + 2 }
        
        (exprVal, exprInstrs) <- compileExpr expr
        
        stmtInstrs <- compileStmt stmt
        
        stateAfterStmt <- get
        let exitLabel = LLVMLab (nextLabel stateAfterStmt)
            brInstrs = [LLVMBrCond exprVal (LLVMLab (nextLabel state + 1)) exitLabel,
                        LLVMLabel (LLVMLab (nextLabel state + 1))]
        
        let phiInstrsFinal = updatePhiInstrs phiInstrs differingVars stateAfterStmt (LLVMLab (nextLabel stateAfterStmt - 1))
        
        put stateAfterStmt { nextLabel = nextLabel stateAfterStmt + 1, varEnv = varEnv state, funEnv = funEnv state, store = store state, strLoadReg = strLoadReg state }
        
        return $ loadInstrs ++ beginInstrs ++ phiInstrsFinal ++ exprInstrs ++
                 brInstrs ++ stmtInstrs ++ [LLVMBr (LLVMLab (nextLabel state))] ++ [LLVMLabel exitLabel]
        where
            modifyStoreEntry :: Integer -> LLVMVal -> (Store, Integer) -> (Store, Integer)
            modifyStoreEntry loc val (accStore, currentNextReg)
                | isRegVal val = (accStore, currentNextReg)
                | otherwise =
                    let newRegVal = RegVal (getTypeFromVal val) (LLVMReg currentNextReg)
                        newStore = Data.Map.insert loc newRegVal accStore
                        newNextReg = currentNextReg + 1
                    in (newStore, newNextReg)
            
            isRegVal :: LLVMVal -> Bool
            isRegVal (RegVal _ _) = True
            isRegVal _ = False
            
            generatePhiInstrs :: ([LLVMInstr], [LLVMInstr]) -> Integer -> CompilerMonad ([LLVMInstr], [LLVMInstr])
            generatePhiInstrs (phiAcc, loadAcc) loc = do
                state <- get
                (val, loadInstrs) <- fixStringVal (fromJust (Data.Map.lookup loc (store state)))
                let reg = nextReg state
                    typ = getTypeFromVal val
                    phiInstr = LLVMPhi (LLVMReg reg) typ [(val, LLVMLab (nextLabel state - 1))]
                put state { nextReg = nextReg state + 1 , store = Data.Map.insert loc (RegVal typ (LLVMReg reg)) (store state) }
                return (phiAcc ++ [phiInstr], loadAcc ++ loadInstrs)
            
            updatePhiInstrs :: [LLVMInstr] -> [Integer] -> CompilerState -> LLVMLab -> [LLVMInstr]
            updatePhiInstrs phiInstrs differingVars state lab =
                [ updatePhiInstr phi loc | (phi, loc) <- zip phiInstrs differingVars ]
                where
                    updatePhiInstr :: LLVMInstr -> Integer -> LLVMInstr
                    updatePhiInstr (LLVMPhi reg typ list) loc =
                        let val = fromJust (Data.Map.lookup loc (store state))
                            updatedList = list ++ [(val, lab)]
                        in LLVMPhi reg typ updatedList
                    updatePhiInstr instr _ = instr
    
    compileStmt (SExp _ expr) = do
        (_, instrs) <- compileExpr expr
        return instrs
