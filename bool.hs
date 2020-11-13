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
                  (f `Or` C True) -> C True
                  (C True `Or` f) -> C True
                  (Not (C False)) -> C True
                  (Not (C True)) -> C False
                  f -> f

simplifyConst :: Form -> Form
simplifyConst (f1 `And` f2) = removeConst ((simplifyConst f1) `And` (simplifyConst f2)) 
simplifyConst (f1 `Or` f2) = removeConst ((simplifyConst f1) `Or` (simplifyConst f2)) 
simplifyConst (Not f) = removeConst (simplifyConst f)
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
fvList (Not (V x)) = [x]
fvList (V x) = [x]
fvList (C _) = []
fvList (Not f) = fvList f
fvList (Not f1 `And` f2) = (fvList f1) ++ (fvList f2)
fvList (Not f1 `Or` f2) = (fvList f1) ++ (fvList f2)

fv :: Form -> [String]
fv f = nub $ fvList $ nnf f

subst :: Form -> (String, Bool) -> Form
subst (V x)(y, b)
  | (x == y) = (C b)
  | (x /= y) = (V x)
subst (Not (V x))(y, b) = Not (subst (V x)(y, b))
subst (C b)(y, bl) = (C b)
subst (Not (C b))(y, bl) = Not (C b)
subst ((V x) `And` f)(y, b)
  | (x == y) = ((C b) `And` (subst f(y, b)))
  | (x /= y) = ((V x) `And` (subst f(y, b)))
subst ((V x) `Or` f)(y, b)
  | (x == y) = ((C b) `Or` (subst f(y, b)))
  | (x /= y) = ((V x) `Or` (subst f(y, b)))
subst ((Not (V x)) `And` f)(y, b) = Not (subst ((V x) `And` f)(y, b))
subst ((Not (V x)) `Or` f)(y, b) = Not (subst ((V x) `Or` f)(y, b))
subst ((C b) `And` f)(y, bl) = ((C b) `And` (subst f(y, bl)))
subst ((C b) `Or` f)(y, bl) = ((C b) `Or` (subst f(y, bl)))
subst ((Not (C b)) `And` f)(y, bl) = Not ((C b) `And` f)(y, bl))
subst ((Not (C b)) `Or` f)(y, bl) = Not ((C b) `Or` f)(y, bl))

substAll :: Form -> [(String, Bool)] -> Form
substAll = foldl subst

getBool :: Form -> Bool
getBool f
  | f == C True = True
  | f == C False = False

evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst f l = getBool $ simplifyConst $ (substAll (nnf f) l)

models :: Form -> [(String, Bool)] -> [String] -> [[(String, Bool)]]
models f vl (vn:vns) = (models f ((vn, True):vl) vns) ++ (models f ((vn, False):vl) vns)
models f vl []
  | (evalSubst f vl) == True = [vl]
  | otherwise = []

allModels :: Form -> [[(String, Bool)]]
allModels f = models f [] (fv f)

unsatisfiable :: Form -> Bool
unsatisfiable f = null (allModels f)

valid :: Form -> Bool
valid f = (unsatisfiable (Not f))
