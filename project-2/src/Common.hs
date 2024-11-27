module Common where
    showPosition :: Maybe (Int, Int) -> String
    showPosition Nothing = "position unknown"
    showPosition (Just (row, col)) = "row: " ++ (show row) ++ ", column: " ++ (show col)
