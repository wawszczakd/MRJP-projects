module Main where
    import System.Environment
    import System.Exit
    import System.FilePath
    import System.IO
    import System.Process
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
                        let
                            code = compileProgramJVM program file
                            jFile = replaceExtension file "j"
                        writeFile jFile code
                        putStrLn $ "JVM code has been written to " ++ jFile
                        _ <- system $ "java -jar lib/jasmin.jar " ++ jFile
                        return ()
            _ -> do
                putStrLn "Invalid usage! Give a file as an argument."
                exitFailure
