module Main where
    import System.Environment
    import System.Exit
    import System.IO
    import ParInstant
    import CompilerJVM
    
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
                        putStrLn $ compileProgramJVM program
            _ -> do
                putStrLn "Invalid usage! Give a file as an argument."
                exitFailure
