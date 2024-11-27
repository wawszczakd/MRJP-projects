module Main where
    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.State
    import Data.Map
    import ParLatte
    import System.Environment
    import System.Exit
    import System.FilePath
    import System.IO
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
                            Right () -> putStrLn $ "Type checking succesful."
                            Left err -> putStrLn $ "Type checking failed: " ++ err
            _ -> do
                putStrLn "Invalid usage! Give a file as an argument."
                exitFailure
