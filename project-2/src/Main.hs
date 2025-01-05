module Main where
    import Compiler
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.List
    import Data.Map
    import LLVMInstructions
    import ParLatte
    import System.Directory
    import System.Environment
    import System.Exit
    import System.FilePath
    import System.Process
    import TypeChecker
    
    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [file] -> do
                input <- readFile file
                let parsingResult = pProgram $ myLexer input
                case parsingResult of
                    (Left err) -> do
                        putStrLn err
                        exitFailure
                    (Right program) -> do
                        typeCheckingResult <- runExceptT $ runReaderT (checkProgram program) (Data.Map.empty, Data.Map.empty)
                        case typeCheckingResult of
                            Right () -> do
                                putStrLn "Type checking successful."
                                
                                instrs <- compileProgram program
                                let
                                    code = unlines (Data.List.map show instrs)
                                    llFile = replaceExtension file "ll"
                                    bcFile = replaceExtension file "bc"
                                writeFile llFile code
                                putStrLn $ "LLVM code has been written to " ++ llFile
                                
                                exeDir <- takeDirectory <$> getExecutablePath
                                let runtimePath = exeDir </> "lib/runtime.bc"
                                
                                _ <- system $ "llvm-link " ++ llFile ++ " " ++ runtimePath ++ " -o " ++ bcFile
                                putStrLn $ "Generated: " ++ bcFile
                            Left err -> putStrLn $ "Type checking failed: " ++ err
            _ -> do
                putStrLn "Invalid usage! Provide a file as an argument."
                exitFailure
