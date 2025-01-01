module LLVMInstructions where
    import Data.List
    
    data LLVMType = LLVMInt | LLVMStr | LLVMBool | LLVMVoid deriving Eq
    instance Show LLVMType where
        show LLVMInt  = "i32"
        show LLVMStr  = "i8*"
        show LLVMBool = "i1"
        show LLVMVoid = "void"
    
    data LLVMBinOp = LLVMPlus | LLVMMinus | LLVMTimes | LLVMDiv | LLVMMod | LLVMXor | LLVMLTH | LLVMLE | LLVMGTH | LLVMGE | LLVMEQU | LLVMNE | LLVMAnd | LLVMOr
    instance Show LLVMBinOp where
        show LLVMPlus  = "add"
        show LLVMMinus = "sub"
        show LLVMTimes = "mul"
        show LLVMDiv   = "sdiv"
        show LLVMMod   = "srem"
        show LLVMXor   = "xor"
        show LLVMLTH   = "slt"
        show LLVMLE    = "sle"
        show LLVMGTH   = "sgt"
        show LLVMGE    = "sge"
        show LLVMEQU   = "icmp eq"
        show LLVMNE    = "icmp ne"
        show LLVMAnd   = "and"
        show LLVMOr    = "or"
    
    data LLVMReg = LLVMReg Integer
    instance Show LLVMReg where
        show (LLVMReg reg) = "%R" ++ show reg
    
    data LLVMArgDec = LLVMArgDec LLVMType
    instance Show LLVMArgDec where
        show (LLVMArgDec typ) = show typ
    
    data LLVMArg = LLVMArg LLVMType LLVMReg
    instance Show LLVMArg where
        show (LLVMArg typ reg) = show typ ++ " " ++ show reg
    
    data LLVMVal = IntVal Integer | StrVal String | BoolVal Bool | RegVal LLVMType LLVMReg
    instance Show LLVMVal where
        show (IntVal n)       = show n
        show (BoolVal b)      = if b then "1" else "0"
        show (StrVal s)       = show s
        show (RegVal typ reg) = show reg
    
    data LLVMValT = LLVMValT LLVMVal
    instance Show LLVMValT where
        show (LLVMValT (IntVal n))       = "i32 " ++ show n
        show (LLVMValT (BoolVal b))      = "i1 " ++ if b then "1" else "0"
        show (LLVMValT (StrVal s))       = "i8* " ++ show s
        show (LLVMValT (RegVal typ reg)) = show typ ++ " " ++ show reg
    
    toLLVMValT :: LLVMVal -> LLVMValT
    toLLVMValT val = LLVMValT val
    
    data LLVMInstr =
        LLVMEmpty
        | LLVMFunDec LLVMType String [LLVMArgDec]
        | LLVMFunDef LLVMType String [LLVMArg] [LLVMInstr]
        | LLVMRet LLVMValT
        | LLVMRetVoid
        | LLVMCall LLVMType String [LLVMValT]
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
