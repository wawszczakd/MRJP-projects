module UtilsTypeChecker where
    import AbsLatte
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
    
    data MyType = MyInt | MyStr | MyBool | MyVoid deriving Eq
    data MyFun = MyFun MyType [MyType]
    
    data ValueState = FixedInt Integer | FixedBool Bool | FixedString String | Unknown
    
    -- (function map, variable map) --
    type Env = (Data.Map.Map Ident MyFun, Data.Map.Map Ident (MyType, Integer))
    
    type TypeCheckerMonad = ReaderT Env (ExceptT String IO)
    
    toMyType :: Type -> MyType
    toMyType (Int _) = MyInt
    toMyType (Str _) = MyStr
    toMyType (Bool _) = MyBool
    toMyType (Void _) = MyVoid
    
    argToType :: Arg -> MyType
    argToType (Arg _ typ _) = toMyType typ
    
    showPosition :: Maybe (Int, Int) -> String
    showPosition Nothing = "position unknown"
    showPosition (Just (line, col)) = "line: " ++ show line ++ ", column: " ++ show col
