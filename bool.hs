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
simplifyConst (Not f) = removeConst (Not (simplifyConst f))
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
fvList (V x) = [x]
fvList (C _) = []
fvList (f1 `And` f2) = (fvList f1) ++ (fvList f2)
fvList (f1 `Or` f2) = (fvList f1) ++ (fvList f2)
fvList (Not f) = fvList f

fv :: Form -> [String]
fv f = nub $ fvList $ nnf f

subst :: Form -> (String, Bool) -> Form
subst (V x)(y, b)
  | (x == y) = (C b)
  | (x /= y) = (V x)
subst (C b)(y, bl) = (C b)
subst (f1 `And` f2)(y, b) = (subst f1(y, b) `And` subst f2(y, b))
subst (f1 `Or` f2)(y, b) = (subst f1(y, b) `Or` subst f2(y, b))
subst (Not f)(y, b) = Not (subst f(y, b))

substAll :: Form -> [(String, Bool)] -> Form
substAll = foldl subst

getBool :: Form -> Bool
getBool f
  | f == C True = True
  | f == C False = False

evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst f l = getBool $ simplifyConst $ (substAll (nnf f) l)

models :: Form -> [(String, Bool)] -> [String] -> [[(String, Bool)]]
models f vl (vn:vns)
  = (models f ((vn, True):vl) vns) ++ (models f ((vn, False):vl) vns)
models f vl []
  | (evalSubst f vl) == True = [vl]
  | otherwise = []

allModels :: Form -> [[(String, Bool)]]
allModels f = models f [] (fv f)

unsatisfiable :: Form -> Bool
unsatisfiable f = null (allModels f)

valid :: Form -> Bool
valid f = (unsatisfiable (Not f))

-- propositional prolog

data Rule
  = Rl String [String]
  deriving (Eq, Show)

data Goal
  = Gl [String]
  deriving (Eq, Show)

data Prog
  = Pr [Rule] Goal
  deriving (Eq, Show)

mortalSocrates :: Prog
mortalSocrates
  = Pr
  [Rl "h" [],
   Rl "m" ["h"]
  ]
  (Gl ["m"])

immortalSocrates :: Prog
immortalSocrates
  = Pr
  [Rl "h" [],
   Rl "h" ["m"]
  ]
  (Gl ["m"])

abcdProg :: Prog
abcdProg
  = Pr
  [Rl "a" [],
   Rl "d" ["b", "c"],
   Rl "d" ["a", "c"],
   Rl "c" ["a"]
  ]
  (Gl ["d", "c"])

implies :: Form -> Form -> Form
implies f g = (Not f) `Or` g

conj :: [Form] -> Form
conj [] = C True
conj (f:fs) = foldl (\x y -> x `And` y) f fs 

getVar :: Rule -> String
getVar (Rl v _) = v

getString :: Rule -> [String] 
getString (Rl _ s) = s 

getHead :: Rule -> Form
getHead r =
  V (getVar r)

getBody :: Rule -> [Form]
getBody r =
  fmap V (getString r)

ruleToForm :: Rule -> Form
ruleToForm r = implies (conj $ getBody r) (getHead r)

goalToForm :: Goal -> Form
goalToForm (Gl v) = conj $ fmap V v

progToForm :: Prog -> Form
progToForm (Pr r g) =
  implies (conj $ fmap ruleToForm r) (goalToForm g)

-- interlude

divls = [1,4,5,9]
anydiv3 l = any (\x -> mod x 3 == 0) l
alldiv3 l = all (\x -> mod x 3 == 0) l

sqls = [1, -3, 4, -5]
sqneg l = [(x^2) | x <- l, x < 0]

getRule :: Prog -> [Rule]
getRule (Pr r _) = r

getGoal :: Prog -> [String]
getGoal (Pr _ (Gl g)) = g

findHead :: [Rule] -> String -> [Rule]
findHead rls h =
  [r | r <- rls, (getVar r) == h]

-- running prop prolog progs

solveProp :: [Rule] -> String -> Bool
solveProp rls p =
  any (\x -> getVar x == p) rls

solveGoal :: [Rule] -> [String] -> Bool
solveGoal rls ps =
  all (\x -> (solveProp rls x) == True) ps

runProg :: Prog -> Bool
runProg p =
  solveGoal (getRule p) (getGoal p)

nonterminating :: Prog
nonterminating =
  Pr
  [Rl "h" [],
   Rl "m" ["m", "h"],
   Rl "m" ["h"]
  ]
  (Gl ["m"])

terminating :: Prog
terminating
  = Pr
  [Rl "h" [],
   Rl "m" ["h"],
   Rl "m" ["m", "h"]
  ]
  (Gl ["m"])
