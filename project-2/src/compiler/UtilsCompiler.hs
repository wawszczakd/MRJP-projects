module UtilsCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    import LLVMInstructions
    
    -- maps functions to return types --
    type FunEnv = Data.Map.Map Ident LLVMType
    
    -- maps variables to locations --
    type VarEnv = Data.Map.Map Ident Integer
    
    -- maps locations to registers --
    type Store = Data.Map.Map Integer LLVMVal
    
    data CompilerState = CompilerState {
        nextLoc    :: Integer, -- next available location
        nextReg    :: Integer, -- next available register
        nextLabel  :: Integer, -- next available label
        funEnv     :: FunEnv,
        varEnv     :: VarEnv,
        store      :: Store,
        strDec     :: Data.Map.Map String Integer,
        strLoadReg :: Data.Map.Map String LLVMReg,
        nextStr    :: Integer
    }
    
    type CompilerMonad = StateT CompilerState IO
    
    typeToLLVM :: Type -> LLVMType
    typeToLLVM (Int _) = LLVMInt
    typeToLLVM (Str _) = LLVMStr
    typeToLLVM (Bool _) = LLVMBool
    typeToLLVM (Void _) = LLVMVoid
    
    getStrReg :: String -> CompilerMonad (LLVMReg, [LLVMInstr])
    getStrReg s = do
        insertToStrDec s
        state <- get
        let Just n = Data.Map.lookup s (strDec state)
        case Data.Map.lookup s (strLoadReg state) of
            Just reg -> return (reg, [])
            Nothing -> do
                let reg = LLVMReg (nextReg state)
                    newStrLoadReg = Data.Map.insert s reg (strLoadReg state)
                put state { nextReg = nextReg state + 1, strLoadReg = newStrLoadReg }
                return (reg, [LLVMLoadStr reg (LLVMString s n)])
        where
            insertToStrDec :: String -> CompilerMonad ()
            insertToStrDec s = do
                state <- get
                case Data.Map.lookup s (strDec state) of
                    Just _ -> return ()
                    Nothing -> do
                        let newStrDec = Data.Map.insert s (nextStr state) (strDec state)
                        put state { strDec = newStrDec, nextStr = nextStr state + 1 }
                        return ()
    
    getTypeFromVal :: LLVMVal -> LLVMType
    getTypeFromVal (IntVal _) = LLVMInt
    getTypeFromVal (StrVal _) = LLVMStr
    getTypeFromVal (BoolVal _) = LLVMBool
    getTypeFromVal (RegVal typ _) = typ
    
    fixStringVal :: LLVMVal -> CompilerMonad (LLVMVal, [LLVMInstr])
    fixStringVal (StrVal s) = do
        (reg, instrs) <- getStrReg s
        return (RegVal LLVMStr reg, instrs)
    fixStringVal val = return (val, [])
