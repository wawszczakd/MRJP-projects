module Main where
    import System.Environment
    import System.Exit
    import ParInstant
    import CompilerLLVM
    
    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [f] -> do
                input <- readFile f
                let result = pProgram $ myLexer input
                case result of
                    (Left err) -> do
                        putStrLn err
                        exitFailure
                    (Right program) -> do
                        putStrLn $ compileProgram program
            _ -> do
                putStrLn "Invalid usage! Give a file as an argument."
                exitFailure
