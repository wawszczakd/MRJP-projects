module Main where
    import Compiler
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
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
                let result = pProgram $ myLexer input
                case result of
                    (Left err) -> do
                        putStrLn err
                        exitFailure
                    (Right program) -> do
                        res <- runExceptT $ runReaderT (checkProgram program) Data.Map.empty
                        case res of
                            Right () -> do
                                putStrLn "Type checking successful."
                                let
                                    code = compileProgram program
                                    llFile = replaceExtension file "ll"
                                    bcFile = replaceExtension file "bc"
                                writeFile llFile code
                                putStrLn $ "LLVM code has been written to " ++ llFile
                                
                                exeDir <- takeDirectory <$> getExecutablePath
                                let runtimePath = exeDir </> "lib/runtime.bc"
                                
                                _ <- system $ "llvm-as " ++ llFile ++ " -o tmp.bc"
                                _ <- system $ "llvm-link tmp.bc " ++ runtimePath ++ " -o " ++ bcFile
                                _ <- system "rm tmp.bc"
                                putStrLn $ "Generated: " ++ bcFile
                            Left err -> putStrLn $ "Type checking failed: " ++ err
            _ -> do
                putStrLn "Invalid usage! Provide a file as an argument."
                exitFailure
