import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

-- TOTAL: 23.5/25 (94%)
------------------------------------------------------
-- PART I: 7/7

-- 3/3
-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k t
  = (snd . head) (filter ((==k) . fst) t)

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp k d t
  | null filtered = d
  | otherwise     = (snd . head) filtered
  where
    filtered = filter ((==k) . fst) t

-- 2/2
-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp k t
  = map fst (filter ((==k) . snd) t)

-- 2/2
occurs :: String -> Type -> Bool
occurs var (TFun t t')
  = occurs var t || occurs var t'
occurs var (TVar x)
  = var == x
occurs _ _
  = False

------------------------------------------------------
-- PART II: 10/10

-- 10/10
-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _
  = TInt 
inferType (Boolean _) _
  = TBool
inferType (Id x) env
  = lookUp x env 
inferType (Prim f) env
  = lookUp f primTypes
inferType (Cond e e' e'') env
  | inferType e env == TBool && t == inferType e'' env = t 
  | otherwise                                          = TErr
    where
      t = inferType e' env
inferType (App f a) env
  = inferApp (inferType f env) (inferType a env) 
  where
    inferApp :: Type -> Type -> Type
    inferApp (TFun t t') tA
      | t == tA   = t'
    inferApp _ _
      = TErr


    
    
------------------------------------------------------
-- PART III: 10/10

-- 2/2
applySub :: Sub -> Type -> Type
applySub s (TVar x)
  = tryToLookUp x (TVar x) s
applySub s (TFun t t')
  = TFun (applySub s t) (applySub s t')
applySub _ t
  = t

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

-- 8/8
unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s
  = Just s
unifyPairs ((TInt, TInt) : ps) s
  = unifyPairs ps s
unifyPairs ((TBool, TBool) : ps) s
  = unifyPairs ps s
unifyPairs ((TVar v, TVar v') : ps) s
  | v == v' = unifyPairs ps s
unifyPairs ((TVar v, t) : ps) s
  = unifyPairs' v t ps s
unifyPairs ((t, TVar v) : ps) s
  = unifyPairs' v t ps s
unifyPairs ((TFun t1 t2, TFun t1' t2') : ps) s
  = unifyPairs ((t1, t1') : (t2, t2') : ps) s
unifyPairs _ _
  = Nothing

unifyPairs' :: String -> Type -> [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs' v t ps s
  | occurs v t = Nothing
  | otherwise  = unifyPairs (map (\(x, y) -> (appSub x, appSub y)) ps) ((v, t) : s)
  where
    appSub = applySub [(v, t)]


------------------------------------------------------
-- PART IV: 0.5/3

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

-- 0.5/3 Decent attempt
-- inferPolyType :: Expr -> Type
-- inferPolyType (Number _)
--   = TInt 
-- inferPolyType (Boolean _)
--   = TBool
-- inferPolyType (Prim f)
--   = lookUp f primTypes
-- inferPolyType (Id x) -- See this needs an env so this hints that all of these cases should be in the helper function
--   = lookUp x env 
-- inferPolyType (Fun x e)
--   | te == TErr = TErr
--   | otherwise  = TFun (applySub s (TVar (show i))) te
--   where
--     (s, te, i) = inferPolyType' e [(x, TVar (show 0))] 0

-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

-- inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
-- inferPolyType'
--   = undefined

-- inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
-- inferPolyType' e env i
--   = (fromJust s, te, i + 1)
--   where
--     s = unify (TVar (show i)) te
--     te         = inferPolyType e

isError :: Type -> Bool
isError TErr
  = True
isError (TFun t t')
  = isError t || isError t'
isError _
  = False

inferPolyType :: Expr -> Type
inferPolyType e 
  | isError te = TErr
  | otherwise  = te 
  where
    (_, te, _) = inferPolyType' e [] 0 

-- * When struggling with a question, use the helper function more
inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
inferPolyType' (Number _) _ i 
  = ([], TInt, i)
inferPolyType' (Boolean _) _ i
  = ([], TBool, i)
inferPolyType' (Prim f) _ i
  = ([], lookUp f primTypes, i)
inferPolyType' (Id x) env i 
  = ([], lookUp x env, i)
inferPolyType' (Fun x e) env i
  = (s', TFun (applySub s (TVar (show i))) te, i')
    where
      env'        = (x, TVar (show i)) : env
      (s, te, i') = inferPolyType' e ((x, TVar (show i)) : env) (i + 1)
      s'          = (x, TVar (show i)) : s -- Is this needed? It is in the GitHub solution but I don't see how this is used
inferPolyType' (App f e) env i
  | isNothing maybeS = ([], TErr, i'')
  | otherwise        = (combineSubs [sU, s', s], applySub sU (TVar (show i)), i'')
    where
      (s, te, i') = inferPolyType' e env i
      env'        = updateTEnv env s
      (s', tf, i'') = inferPolyType' f env' i'
      tf'           = applySub (s ++ s') tf
      maybeS        = unify tf' (TFun te (TVar (show i)))
      sU            = fromJust maybeS
------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))

ex15 = Fun "x" (Fun "y" (App (App (Prim "+") (Id "y")) (Id "x")))
type15 = TFun TInt (TFun TInt TInt)

ex16 = Fun "x" (Fun "y" (App (App (Prim "+") (Id "y")) (App (Prim "not") (Id "x"))))
type16 = TErr