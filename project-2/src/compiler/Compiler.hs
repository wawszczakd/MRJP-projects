module Compiler where
    import AbsLatte
    import Control.Monad.State
    import Data.List
    import Data.Map
    import UtilsCompiler
    
    compileProgram :: Program -> String
    compileProgram (Prog _ topDefs) =
        let
            (programBody, _) = runState (compileTopDefs topDefs) (0, empty)
            programHead = [ "declare i32 @readInt()"
                          , "declare i8* @readString()"
                          , "declare void @printInt(i32)"
                          , "declare void @printString(i8*)" ]
        in
            unlines (programHead ++ programBody)
    
    compileTopDef :: TopDef -> CompilerMonad [String]
    compileTopDef (TopFunDef _ (FnDef _ typ (Ident name) args block)) = do
        let retType = compileType typ
        let funcName = name
        let llvmArgs = intercalate ", " (Data.List.map compileArg args)
        let funcHeader = "define " ++ retType ++ " @" ++ funcName ++ "(" ++ llvmArgs ++ ") {"
        funcBody <- compileBlock block
        let funcFooter = "}"
        return $ funcHeader : funcBody ++ [funcFooter]
    
    compileBlock :: Block -> CompilerMonad [String]
    compileBlock (Blck _ stmts) = do
        return ["    ; Function body goes here", "    ret i32 0"]
    
    compileTopDefs :: [TopDef] -> CompilerMonad [String]
    compileTopDefs topDefs = do
        foldM (\acc topDef -> do
                    topDefCode <- compileTopDef topDef
                    return $ acc ++ [""] ++ topDefCode) [] topDefs
