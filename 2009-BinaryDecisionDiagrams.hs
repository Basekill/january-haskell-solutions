import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x ((k, v) : ps) 
  | x == k    = v 
  | otherwise = lookUp x ps

checkSat :: BDD -> Env -> Bool
checkSat (rootID, ns) env
  | rootID == 1 = True
  | rootID == 0 = False
  | rootVal == False = checkSat (leftID, ns) env 
  | otherwise        = checkSat (rightID, ns) env
  where
    (rootIndex, leftID, rightID) = lookUp rootID ns 
    rootVal = lookUp rootIndex env
  

sat :: BDD -> [[(Index, Bool)]]
sat (rootID, ns)
  | rootID == 1 = [[]]
  | rootID == 0 = []
  | otherwise   = [(rootIndex, False) : set | set <- sat (leftID, ns)] ++
                  [(rootIndex, True) : set | set <- sat (rightID, ns)]
  where
    (rootIndex, leftID, rightID) = lookUp rootID ns


-- sat :: BDD -> [[(Index, Bool)]]
-- sat bdd
--   = sat' bdd []
--   where
--     sat' :: BDD -> [[(Index, Bool)]] -> [[(Index, Bool)]]
--     sat' (rootID', ns') sets
--       | rootID' == 1 = [] : sets 
--       | rootID' == 0 = sets 
--       | length sets > 0 = sat' (leftID, ns') [(rootIndex, False) : set | set <- sets] ++
--                           sat' (rightID, ns') [(rootIndex, True) : set | set <- sets] 
--       | otherwise       = sat' (leftID, ns') [[(rootIndex, False)]] ++
--                           sat' (rightID, ns') [[(rootIndex, True)]]
--       where
--         (rootIndex, leftID, rightID) = lookUp rootID' ns'
------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim x))
  = Prim (not x) 
simplify (And (Prim x) (Prim x'))
  = Prim (x && x')
simplify (Or (Prim x) (Prim x'))
  = Prim (x || x')
simplify bexp
  = bexp

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef x) index bool
  | x == index = Prim bool 
  | otherwise  = IdRef x
restrict (Prim x) _ _
  = Prim x
restrict (Not bexp) index bool
  = simplify (Not (restrict bexp index bool))
restrict (And bexp bexp') index bool
  = simplify (And (restrict bexp index bool) (restrict bexp' index bool))
restrict (Or bexp bexp') index bool
  = simplify (Or (restrict bexp index bool) (restrict bexp' index bool))


------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs 
  = buildBDD' e 2 xs 

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' e _ []
  = (b, [])
  where
    b = if fromPrim e then 1 else 0 
buildBDD' e currentID (x : xs)
  = (currentID, (currentID, (x, leftID, rightID)) : leftNodes ++ rightNodes)
  where
    (leftID, leftNodes) = buildBDD' (restrict e x False) (2 * currentID) xs
    (rightID, rightNodes) = buildBDD' (restrict e x True) (2 * currentID + 1) xs

fromPrim :: BExp -> Bool 
-- Pre: BExp must be a Prim
fromPrim (Prim x)
  = x



------------------------------------------------------
-- PART IV
buildROBDD e xs
  = buildROBDD' e 2 xs 

buildROBDD' :: BExp -> NodeId -> [Index] -> BDD
buildROBDD' e _ []
  = (b, [])
  where
    b = if fromPrim e then 1 else 0
buildROBDD' e currentID (x : xs)
  | leftID == rightID = (2 * currentID, leftNodes)
  | otherwise = (currentID, (currentID, (x, leftID, rightID)) : leftNodes ++ rightNodes)
  where
      (leftID, leftNodes) = buildBDD' (restrict e x False) (2 * currentID) xs
      (rightID, rightNodes) = buildBDD' (restrict e x True) (2 * currentID + 1) xs



------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])

