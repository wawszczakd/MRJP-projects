module Main where
    import System.Environment
    import System.Exit
    import System.FilePath
    import System.IO
    import System.Process
    import ParInstant
    import CompilerLLVM
    
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
                        let
                            code = compileProgramLLVM program
                            llFile = replaceExtension file "ll"
                            bcFile = replaceExtension file "bc"
                        withFile llFile WriteMode $ \handle -> do
                            hPutStrLn handle code
                        putStrLn $ "LLVM code has been written to " ++ llFile
                        _ <- runCommand $ "llvm-as " ++ llFile ++ " -o " ++ bcFile
                        putStrLn $ "Bytecode has been written to " ++ bcFile
            _ -> do
                putStrLn "Invalid usage! Give a file as an argument."
                exitFailure
