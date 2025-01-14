module Optimizations where
    import Data.List
    import Data.Map
    import LLVMInstructions
    
    type ExprMap = Data.Map.Map (LLVMBinOp, LLVMType, LLVMVal, LLVMVal) LLVMReg
    type RegMap = Data.Map.Map LLVMReg LLVMReg
    
    doLCSE :: [LLVMInstr] -> [LLVMInstr]
    doLCSE = Data.List.map optimizeFunDef
        where
            optimizeFunDef :: LLVMInstr -> LLVMInstr
            optimizeFunDef (LLVMFunDef retType name args body) =
                let optimizedBody = go body Data.Map.empty Data.Map.empty [] in
                LLVMFunDef retType name args optimizedBody
            optimizeFunDef instr = instr
            
            go :: [LLVMInstr] -> ExprMap -> RegMap -> [LLVMInstr] -> [LLVMInstr]
            go [] _ _ result = reverse result
            go (instr : instrs) exprMap regMap result =
                let newInstr = applyRegs instr regMap in
                case newInstr of
                    LLVMBin reg op typ val1 val2 ->
                        let exprKey = (op, typ, val1, val2) in
                        case Data.Map.lookup exprKey exprMap of
                            Just tmp ->
                                go instrs exprMap (Data.Map.insert reg tmp regMap) result
                            Nothing ->
                                go instrs (Data.Map.insert exprKey reg exprMap) regMap (newInstr : result)
                    LLVMLabel _ ->
                        go instrs Data.Map.empty Data.Map.empty (newInstr : result)
                    _ ->
                        go instrs exprMap regMap (newInstr : result)
    
    applyRegs :: LLVMInstr -> RegMap -> LLVMInstr
    applyRegs instr regMap = case instr of
        LLVMBin reg op typ val1 val2 -> LLVMBin (applyReg reg) op typ (applyVal val1) (applyVal val2)
        LLVMCallVoid name args -> LLVMCallVoid name (Data.List.map applyValT args)
        LLVMCall reg typ name args -> LLVMCall (applyReg reg) typ name (Data.List.map applyValT args)
        LLVMLoadStr reg str -> LLVMLoadStr (applyReg reg) str
        LLVMPhi reg typ pairs -> LLVMPhi (applyReg reg) typ [(applyVal val, lab) | (val, lab) <- pairs]
        _ -> instr
        where
            applyReg reg = Data.Map.findWithDefault reg reg regMap
            applyVal (RegVal typ reg) = RegVal typ (applyReg reg)
            applyVal val = val
            applyValT (LLVMValT val) = LLVMValT (applyVal val)
