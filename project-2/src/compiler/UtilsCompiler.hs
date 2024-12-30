module UtilsCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    
    data EnvEntry
        = VarEntry { allocReg :: Integer, loadReg :: Integer }
        | FuncEntry { retType :: String }
    
    -- (next available register, environment map) --
    type Env = (Integer, Data.Map.Map Ident EnvEntry)
    
    type CompilerMonad = State Env
    
    data ExprVal = In Integer | Bo Bool | St String | Re Integer
    
    compileArg :: Arg -> String
    compileArg (Ar _ typ (Ident argName)) =
        compileType typ ++ " %" ++ argName
    
    compileType :: Type -> String
    compileType (Int _) = "i32"
    compileType (Str _) = "i8*"
    compileType (Bool _) = "i1"
    compileType (Void _) = "void"
