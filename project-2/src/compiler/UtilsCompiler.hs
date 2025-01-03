module UtilsCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    import LLVMInstructions
    
    -- (functions to return types, variables to locations) --
    type Env = (Data.Map.Map Ident LLVMType, Data.Map.Map Ident Integer)
    
    -- maps locations to registers --
    type Store = Data.Map.Map Integer LLVMVal
    
    -- (next available location, next available register, env, store, last used label, next available label) --
    type CompilerMonad = State (Integer, Integer, Env, Store, LLVMLab, Integer)
    
    typeToLLVM :: Type -> LLVMType
    typeToLLVM (Int _) = LLVMInt
    typeToLLVM (Str _) = LLVMStr
    typeToLLVM (Bool _) = LLVMBool
    typeToLLVM (Void _) = LLVMVoid
