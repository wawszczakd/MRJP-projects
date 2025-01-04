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
        nextLoc   :: Integer, -- next available location
        nextReg   :: Integer, -- next available register
        nextLabel :: Integer, -- next available label
        funEnv    :: FunEnv,
        varEnv    :: VarEnv,
        store     :: Store
    }
    
    type CompilerMonad = State CompilerState
    
    typeToLLVM :: Type -> LLVMType
    typeToLLVM (Int _) = LLVMInt
    typeToLLVM (Str _) = LLVMStr
    typeToLLVM (Bool _) = LLVMBool
    typeToLLVM (Void _) = LLVMVoid
