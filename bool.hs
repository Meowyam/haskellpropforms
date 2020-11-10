data Form
    = C Bool
    | V String
    | Not Form
    | Form `And` Form  
    | Form `Or` Form
    deriving (Eq, Ord, Show, Read)

removeConst :: Form -> Form
removeConst x = case x of
                  (f `And` C True) -> f
                  (C True `And` f) -> f
                  (f `And` C False) -> C False
                  (C False `And` f) -> C False
                  (f `Or` C True) -> f
                  (C True `Or` f) -> f
                  (f `Or` C False) -> f 
                  (C False `Or` f) -> f
                  (Not (C False)) -> C True
                  (Not (C True)) -> C False
                  f -> f
