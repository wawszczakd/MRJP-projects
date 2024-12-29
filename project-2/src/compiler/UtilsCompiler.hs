module UtilsCompiler where
    import AbsLatte
    import Control.Monad.State
    import Data.Map
    
    -- (next available register, map of variables to a pair (allocated register, loaded register)) --
    type CompilerMonad = State (Integer, Data.Map.Map String (Integer, Integer))
    
    data ExprVal = Num Integer | Reg Integer
    
    compileArg :: Arg -> String
    compileArg (Ar _ typ (Ident argName)) =
        compileType typ ++ " %" ++ argName
    
    compileType :: Type -> String
    compileType (Int _) = "i32"
    compileType (Str _) = "i8*"
    compileType (Bool _) = "i1"
    compileType (Void _) = "void"
