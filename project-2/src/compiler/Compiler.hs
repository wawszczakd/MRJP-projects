module Compiler where
    import Data.List
    import Data.Map
    import Control.Monad.State
    import AbsLatte
    
    -- (next available register, map of variables to a pair (allocated register, loaded register)) --
    type StateMonad = State (Integer, Data.Map.Map String (Integer, Integer))
    
    data ExprVal = Num Integer | Reg Integer
    
    compileProgram :: Program -> String
    compileProgram (Prog _ topDefs) =
        let
            (programBody, _) = runState (compileTopDefs topDefs) (0, empty)
            programHead = [ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\""
                          , ""
                          , "declare i32 @printf(i8*, ...)"
                          , ""
                          , "define void @printInt(i32 %x) {"
                          , "    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0"
                          , "    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
                          , "    ret void"
                          , "}" ]
        in
            unlines (programHead ++ programBody)
    
    compileTopDef :: TopDef -> StateMonad [String]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args block)) = do
        let retType = compileType typ
        let funcName = name
        let llvmArgs = intercalate ", " (Data.List.map compileArg args)
        let funcHeader = "define " ++ retType ++ " @" ++ funcName ++ "(" ++ llvmArgs ++ ") {"
        funcBody <- compileBlock block
        let funcFooter = "}"
        return $ funcHeader : funcBody ++ [funcFooter]
    
    compileArg :: Arg -> String
    compileArg (Ar _ typ (Ident argName)) =
        compileType typ ++ " %" ++ argName
    
    compileType :: Type -> String
    compileType (Int _) = "i32"
    compileType (Str _) = "i8*"
    compileType (Bool _) = "i1"
    compileType (Void _) = "void"
    
    compileBlock :: Block -> StateMonad [String]
    compileBlock _ = do
        return ["    ; Function body goes here", "    ret i32 0"]
    
    compileTopDefs :: [TopDef] -> StateMonad [String]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [""] ++ topDefCode) [] topDefs
