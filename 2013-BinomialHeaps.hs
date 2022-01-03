import Data.List ((\\), mapAccumR) 

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

-- TOTAL: 24/25 (96%)
--------------------------------------------------------------
-- PART I: 4/4

-- 1/1
key :: BinTree a -> a
key (Node a _ _)
  = a 

rank :: BinTree a -> Int
rank (Node _ r _)
  = r 

children :: BinTree a -> [BinTree a]
children (Node _ _ c)
  = c 

-- combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
-- -- Pre: The two given trees are of the same rank
-- combineTrees t t'@(Node a r c)
--   | key t < key t' = combineTrees t' t 
--   | otherwise      = Node a (r + 1) (t : c)

-- 3/3
combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
-- Pre: The two given trees are of the same rank
combineTrees t@(Node a r c) t'@(Node a' r' c')
  | a < a'    = Node a (r + 1) (t' : c) 
  | otherwise = Node a' (r' + 1) (t : c')


--------------------------------------------------------------
-- PART II: 

-- 2/2
-- extractMin :: Ord a => BinHeap a -> a
-- -- Pre: Heap is non-empty
-- extractMin h
--   = minimum (map key h)

-- * Use point free notation with extensionality to simplify
extractMin :: Ord a => BinHeap a -> a
-- Pre: Heap is non-empty
extractMin
  = minimum . map key

-- 6/6
mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h [] 
  = h
mergeHeaps [] h
  = h
mergeHeaps th@(t : h) th'@(t' : h')
  | rank t < rank t' = t : mergeHeaps h th'  
  | rank t > rank t' = t' : mergeHeaps th h'
  | otherwise = mergeHeaps [combineTrees t t'] (mergeHeaps h h')

-- 1/1
insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v
  = mergeHeaps [Node v 0 []]

-- 5/5
deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps hRemoved h'
    where
      (minT, hRemoved) = removeMin h
      h' = (reverse . children) minT 

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove _ []
  = []
remove v (h : hs)
  | key h == v = hs
  | otherwise  = h : (remove v hs)

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin h
  = (head (h \\ h'), h')
  where
    h' = remove (extractMin h) h 

-- 4/4
-- binSort :: Ord a => [a] -> [a]
-- binSort xs
--   = binSort'' (binSort' xs [])
--     where
--       binSort' :: Ord a => [a] -> BinHeap a -> BinHeap a
--       binSort' [] h 
--         = h 
--       binSort' (x : xs) h
--         = mergeHeaps (insert x h) (binSort' xs h)
--       binSort'' :: Ord a => BinHeap a -> [a]
--       binSort'' []
--         = []
--       binSort'' h
--         = extractMin h : binSort'' (deleteMin h)

-- binSort :: Ord a => [a] -> [a]
-- binSort xs
--   = binSort' (foldl mergeHeaps [] (map (flip insert []) xs)) 
--     where
--       binSort' :: Ord a => BinHeap a -> [a]
--       binSort' []
--         = []
--       binSort' h
--         = extractMin h : binSort' (deleteMin h)

-- * Using foldr we can remove the map since we are applying f to x u rather than u x
binSort :: Ord a => [a] -> [a]
binSort xs
  = binSort' (foldr (mergeHeaps . flip insert []) [] xs) 
    where
      binSort' :: Ord a => BinHeap a -> [a]
      binSort' []
        = []
      binSort' h
        = extractMin h : binSort' (deleteMin h)

-- * Github solution using a very neat unfoldr
-- * in a Just (a, b) with the unfoldr, the a is prepended to the list and b is used as the next recursive call
-- makeHeap :: Ord a => [a] -> BinHeap a
-- makeHeap
--   = foldr (mergeHeaps . flip insert []) []
-- 
-- sortHeap :: Ord a => BinHeap a -> [a]
-- sortHeap
--   = unfoldr maybeMin
--   where
--     maybeMin h'
--       | null h'   = Nothing
--       | otherwise = Just (extractMin h', deleteMin h')
-- 
-- binSort :: Ord a => [a] -> [a]
-- binSort
--   = sortHeap . makeHeap



--------------------------------------------------------------
-- PART III

-- 2/3 - Missing pattern for toBinary and used reverse in binarySum
toBinary :: BinHeap a -> [Int]
-- * Must add pattern for empty heap or write a precondition
toBinary []
  = [0]
toBinary h
  = toBinary' (maximum rs) 
    where
      rs = map rank h
      toBinary' :: Int -> [Int]
      toBinary' n
        | n < 0     = []
        | elem n rs = 1 : (toBinary' (pred n))
        | otherwise = 0 : (toBinary' (pred n))

fullAdder :: Int -> Int -> Int -> (Int, Int)
fullAdder b b' c
  | sum == 0  = (0, 0)
  | sum == 1  = (1, 0)
  | sum == 2  = (0, 1)
  | otherwise = (1, 1)
  where
    sum = b + b' + c


-- A bit more efficient but uses reverse
binarySum :: [Int] -> [Int] -> [Int]
binarySum ds ds' 
  = reverse (binarySum' 0 (reverse ds) (reverse ds'))
  where
    binarySum' :: Int -> [Int] -> [Int] -> [Int]
    binarySum' c (d : ds) (d' : ds')
      = sum : binarySum' c' ds ds' 
      where
        (sum, c') = fullAdder d d' c
    binarySum' c [] []
      | c == 0    = []
      | otherwise = [c]
    binarySum' c ds []
      = binarySum' 0 [c] ds
    binarySum' c [] ds
      = binarySum' 0 [c] ds
      
-- Inefficient but doesn't use reverse
binarySum2 :: [Int] -> [Int] -> [Int]
binarySum2 ds ds' 
  = binarySum2' 0 ds ds'
  where
    binarySum2' :: Int -> [Int] -> [Int] -> [Int]
    binarySum2' c [] []
      | c == 0    = []
      | otherwise = [c]
    binarySum2' c ds []
      = binarySum2' 0 [c] ds
    binarySum2' c [] ds
      = binarySum2' 0 [c] ds
    binarySum2' c ds ds'
      = binarySum2' c' (init ds) (init ds') ++ [sum]
      where
        (sum, c') = fullAdder (last ds) (last ds') c
     
-- * Github solution using mapAccumR
binarySum3 :: [Int] -> [Int] -> [Int]
binarySum3 ds ds'
  | c == 0 = sum
  | c == 1 = 1 : sum
    where
      (c, sum) = mapAccumR carry 0 (binarySum3' ds ds')

carry :: Int -> Int -> (Int, Int)
carry b b'
  | sum == 0 = (0, 0)
  | sum == 1 = (0, 1)
  | sum == 2 = (1, 0)
  | sum == 3 = (1, 1)
  where
    sum = b + b'

binarySum3' :: [Int] -> [Int] -> [Int]
binarySum3' ds ds'
  = uncurry (zipWith (+)) (addZeros ds ds')

addZeros :: [Int] -> [Int] -> ([Int], [Int])
addZeros ds ds'
  | lenDif > 0 = (addedZeros ++ ds, ds')
  | otherwise  = (ds, addedZeros ++ ds')
    where
      addedZeros = replicate (abs lenDif) 0 
      lenDif = length ds' - length ds 

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]


