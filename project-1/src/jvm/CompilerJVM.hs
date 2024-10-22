module CompilerJVM where
    import AbsInstant
    
    compileProgramJVM :: Program -> String
    compileProgramJVM program =
        show program