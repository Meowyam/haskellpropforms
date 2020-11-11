data Form
    = C Bool
    | V String
    | Not Form
    | Form `And` Form  
    | Form `Or` Form
    deriving (Eq, Ord, Show, Read)

removeConst :: Form -> Form
removeConst x = case x of
                  (f `And` C False) -> C False
                  (C False `And` f) -> C False
                  (f `And` C True) -> f
                  (C True `And` f) -> f
                  (f `Or` C False) -> f 
                  (C False `Or` f) -> f
                  (f `Or` C True) -> f
                  (C True `Or` f) -> f
                  (Not (C False)) -> C True
                  (Not (C True)) -> C False
                  f -> f

simplifyConst :: Form -> Form
simplifyConst (f1 `And` f2) = removeConst ((simplifyConst f1) `And` (simplifyConst f2)) 
simplifyConst (f1 `Or` f2) = removeConst ((simplifyConst f1) `Or` (simplifyConst f2)) 
simplifyConst f = removeConst f

nnf :: Form -> Form
nnf (Not (f1 `Or` f2)) = nnf (Not f1) `And` nnf (Not f2)
nnf (Not (f1 `And` f2)) = nnf (Not f1) `Or` nnf (Not f2)
nnf (Not (Not f)) = f
nnf (Not f) = Not f
