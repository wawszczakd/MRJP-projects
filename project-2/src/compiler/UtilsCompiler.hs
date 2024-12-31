module UtilsCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    
    data ExprVal = In Integer | Bo Bool | St String | Re Integer
    
    -- (variables to locations, functions to return types) --
    type Env = (Data.Map.Map Ident String, Data.Map.Map Ident Integer)
    
    -- maps locations to registers --
    type Store = Data.Map.Map Integer ExprVal
    
    -- (next available location, next available register, env, store) --
    type CompilerMonad = State (Integer, Integer, Env, Store)
    
    typeToLLVM :: Type -> String
    typeToLLVM (Int _) = "i32"
    typeToLLVM (Str _) = "i8*"
    typeToLLVM (Bool _) = "i1"
    typeToLLVM (Void _) = "void"
