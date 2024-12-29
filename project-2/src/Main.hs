module Main where
    import Compiler
    import Control.Monad.Except
    import Control.Monad.Reader
    import Data.Map
    import ParLatte
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
                                putStrLn "Type checking succesful."
                                print program
                                let
                                    code = compileProgram program
                                    llFile = replaceExtension file "ll"
                                    bcFile = replaceExtension file "bc"
                                writeFile llFile code
                                putStrLn $ "LLVM code has been written to " ++ llFile
                                _ <- system $ "llvm-as " ++ llFile ++ " -o " ++ bcFile
                                putStrLn $ "Generated: " ++ bcFile
                            Left err -> putStrLn $ "Type checking failed: " ++ err
            _ -> do
                putStrLn "Invalid usage! Give a file as an argument."
                exitFailure
