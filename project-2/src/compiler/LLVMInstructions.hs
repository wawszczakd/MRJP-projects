module LLVMInstructions where
    import Data.List
    
    data LLVMType = LLVMInt | LLVMStr | LLVMBool | LLVMVoid
    instance Show LLVMType where
        show LLVMInt  = "i32"
        show LLVMStr  = "i8*"
        show LLVMBool = "i1"
        show LLVMVoid = "void"
    
    data LLVMBinOp = LLVMPlus | LLVMMinus | LLVMTimes | LLVMDiv | LLVMMod
    instance Show LLVMBinOp where
        show LLVMPlus  = "add"
        show LLVMMinus = "sub"
        show LLVMTimes = "mul"
        show LLVMDiv   = "sdiv"
        show LLVMMod   = "srem"
    
    data LLVMRelOp = LLVMLTH | LLVMLE | LLVMGTH | LLVMGE | LLVMEQU | LLVMNE
    instance Show LLVMRelOp where
        show LLVMLTH = "slt"
        show LLVMLE  = "sle"
        show LLVMGTH = "sgt"
        show LLVMGE  = "sge"
        show LLVMEQU = "eq"
        show LLVMNE  = "ne"
    
    data LLVMReg = LLVMReg Integer
    instance Show LLVMReg where
        show (LLVMReg reg) = "%R" ++ (show reg)
    
    data LLVMArg = LLVMArg LLVMType LLVMReg
    instance Show LLVMArg where
        show (LLVMArg typ reg) = (show typ) ++ " " ++ (show reg)
    
    data LLVMVal = NumVal Integer | RegVal LLVMReg
    instance Show LLVMVal where
        show (NumVal n)   = show n
        show (RegVal reg) = show reg
    
    data LLVMInstr =
        LLVMEmpty
        | LLVMFunDec LLVMType String [LLVMArg]
        | LLVMFunDef LLVMType String [LLVMArg] [LLVMInstr]
        | LLVMRet LLVMType LLVMVal
        | LLVMRetVoid
    instance Show LLVMInstr where
        show LLVMEmpty = ""
        show (LLVMFunDec typ name args) =
            "declare " ++ show typ ++ " @" ++ name ++ "(" ++ intercalate ", " (Data.List.map show args) ++ ")"
        show (LLVMFunDef typ name args body) =
            "define " ++ show typ ++ " @" ++ name ++ "(" ++ intercalate ", " (Data.List.map show args) ++ ") {\n" ++
            unlines (map show body) ++ "}"
        show (LLVMRet typ val) =
            "    ret " ++ show typ ++ " " ++ show val
        show LLVMRetVoid =
            "    ret void"
