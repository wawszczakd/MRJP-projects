module Common where
    showPosition :: Maybe (Int, Int) -> String
    showPosition Nothing = "position unknown"
    showPosition (Just (line, col)) = "line: " ++ show line ++ ", column: " ++ show col
