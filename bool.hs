import Data.List

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
nnf (Not (f1 `And` f2)) = nnf (Not f1) `Or` nnf (Not f2)
nnf (Not (f1 `Or` f2)) = nnf (Not f1) `And` nnf (Not f2)
nnf (Not (Not f)) = f
nnf (Not f) = Not f
nnf f = f

distribOr :: Form -> Form -> Form
distribOr f1 f2 = f1 `Or` f2

cnf :: Form -> Form
cnf ((f1 `And` f2) `Or` (g1 `And` g2))
  = (distribOr (cnf f1) (cnf g1)) `And` (distribOr (cnf f2) (cnf g2))
    `And` ((distribOr (cnf f1) (cnf g2)) `And` (distribOr (cnf f2) (cnf g1)))
cnf (f `And` (g1 `Or` g2))
  = (distribOr (cnf f) (cnf g1)) `And` (distribOr (cnf f) (cnf g2))
cnf f = f

-- test
-- cnf $ nnf $ simplifyConst ((C True) `And` (Not (C True) `Or` Not (C False)))
-- C True

fvList :: Form -> [String]
fvList (V x `And` f) = x : fvList f
fvList (V x `Or` f) = x : fvList f
fvList (Not (V x) `And` f) = x : fvList f
fvList (Not (V x) `Or` f) = x : fvList f
fvList (Not (V x)) = x : []
fvList (V x) = x : []

fv :: Form -> [String]
fv = nub . fvList

subst :: Form -> (String, Bool) -> Form
subst ((V x) `And` f) (y, True)
  | (x == y) = (C True `And` (subst f(y, True)))
  | (x /= y) = ((V x) `And` (subst f(y, True)))
subst ((V x) `And` f) (y, False)
  | (x == y) = (C False `And` (subst f)(y, False))
  | (x /= y) = ((V x) `And` (subst f)(y, False))
subst ((V x) `Or` f) (y, True)
  | (x == y) = (C True `Or` (subst f)(y, True))
  | (x /= y) = ((V x) `Or` (subst f)(y, True))
subst ((V x) `Or` f) (y, False)
  | (x == y) = (C False `Or` (subst f)(y, False))
  | (x /= y) = ((V x) `And` (subst f)(y, False))
subst (Not (V x) `And` f) (y, True)
  | (x == y) = (Not (C True) `And` (subst f(y, True)))
  | (x /= y) = ((V x) `And` (subst f(y, True)))
subst (Not (V x) `And` f) (y, False)
  | (x == y) = (Not (C False) `And` (subst f)(y, False))
  | (x /= y) = ((V x) `And` (subst f)(y, False))
subst (Not (V x) `Or` f) (y, True)
  | (x == y) = (Not (C True) `Or` (subst f)(y, True))
  | (x /= y) = ((V x) `Or` (subst f)(y, True))
subst (Not (V x) `Or` f) (y, False)
  | (x == y) = (Not (C False) `Or` (subst f)(y, False))
  | (x /= y) = ((V x) `And` (subst f)(y, False))
subst (Not (V x)) (y, True)
  | (x == y) = Not (C True)
  | (x /= y) = (Not (V x))
subst (Not (V x)) (y, False)
  | (x == y) = Not (C False)
  | (x /= y) = (Not (V x))
subst (V x)(y, True)
  | (x == y) = C True
  | (x /= y) = (V x)
subst (V x)(y, False)
  | (x == y) = C False 
  | (x /= y) = (V x)
