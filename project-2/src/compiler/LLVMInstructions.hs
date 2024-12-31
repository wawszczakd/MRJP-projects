module LLVMInstructions where
    import Data.List
    
    data LLVMType = LLVMInt | LLVMStr | LLVMBool | LLVMVoid deriving Eq
    instance Show LLVMType where
        show LLVMInt  = "i32"
        show LLVMStr  = "i8*"
        show LLVMBool = "i1"
        show LLVMVoid = "void"
    
    data LLVMBinOp = LLVMPlus | LLVMMinus | LLVMTimes | LLVMDiv | LLVMMod | LLVMXor
    instance Show LLVMBinOp where
        show LLVMPlus  = "add"
        show LLVMMinus = "sub"
        show LLVMTimes = "mul"
        show LLVMDiv   = "sdiv"
        show LLVMMod   = "srem"
        show LLVMXor   = "xor"
    
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
        show (LLVMReg reg) = "%R" ++ show reg
    
    data LLVMArgDec = LLVMArgDec LLVMType
    instance Show LLVMArgDec where
        show (LLVMArgDec typ) = show typ
    
    data LLVMArg = LLVMArg LLVMType LLVMReg
    instance Show LLVMArg where
        show (LLVMArg typ reg) = show typ ++ " " ++ show reg
    
    data LLVMVal = IntVal Integer | StrVal String | BoolVal Bool | RegVal LLVMReg
    instance Show LLVMVal where
        show (IntVal n)   = "i32 " ++ show n
        show (BoolVal b)  = "i1 " ++ if b then "1" else "0"
        show (StrVal s)   = "i8* " ++ show s
        show (RegVal reg) = show reg
    
    data LLVMInstr =
        LLVMEmpty
        | LLVMFunDec LLVMType String [LLVMArgDec]
        | LLVMFunDef LLVMType String [LLVMArg] [LLVMInstr]
        | LLVMRet LLVMVal
        | LLVMRetVoid
        | LLVMCall LLVMType String [LLVMVal]
        | LLVMAss LLVMReg LLVMInstr
        | LLVMBin LLVMReg LLVMBinOp LLVMType LLVMVal LLVMVal
    instance Show LLVMInstr where
        show LLVMEmpty = ""
        show (LLVMFunDec typ name args) =
            "declare " ++ show typ ++ " @" ++ name ++ "(" ++ intercalate ", " (Data.List.map show args) ++ ")"
        show (LLVMFunDef typ name args body) =
            "define " ++ show typ ++ " @" ++ name ++ "(" ++ intercalate ", " (Data.List.map show args) ++ ") {\n" ++
            unlines (map show body) ++ "}"
        show (LLVMRet val) =
            "ret " ++ show val
        show LLVMRetVoid =
            "ret void"
        show (LLVMCall typ name args) =
            "call " ++ show typ ++ " @" ++ name ++ "(" ++ intercalate ", " (Data.List.map show args) ++ ")"
        show (LLVMAss reg instr) =
            show reg ++ " = " ++ show instr
        show (LLVMBin reg op typ val1 val2) =
            show reg ++ " = " ++ show op ++ " " ++ show typ ++ " " ++ show val1 ++ ", " ++ show val2
