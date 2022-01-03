import Debug.Trace
import Data.List (inits, tails, sortOn)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------ TOTAL: 20.5/25 (82%)

---------------------------- PART 1: 12/12 ------------------------

-- isPrefix :: String -> String -> Bool
-- isPrefix "" _
--   = True
-- isPrefix _ ""
--   = False
-- isPrefix (c : s) (c' : s')
--   | c == c'   = isPrefix s s' 
--   | otherwise = False

-- * The last bit from above can be simplified
-- isPrefix :: String -> String -> Bool
-- isPrefix "" _
--   = True
-- isPrefix _ ""
--   = False
-- isPrefix (c : s) (c' : s')
--   = c == c' && isPrefix s s' 



-- 1/1
isPrefix :: String -> String -> Bool
isPrefix s s'
  = s == take (length s) s'

-- 1/1
removePrefix :: String -> String -> String
removePrefix s s'
--Pre: s is a prefix of s'
  = drop (length s) s' 

-- * We can simplify removePrefix using extensionality
-- removePrefix :: String -> String -> String
-- removePrefix s
-- --Pre: s is a prefix of s'
--   = drop (length s) 

-- 4/4
suffixes :: [a] -> [[a]]
suffixes s
  = take (length s) (iterate tail s)

-- 3/3
isSubstring :: String -> String -> Bool
isSubstring s s'
  = any (isPrefix s) (suffixes s')

-- findSubstrings :: String -> String -> [Int]
-- findSubstrings s s'
--   = findSubstringsHelper 0 (suffixes s') s 
--     where
--       findSubstringsHelper :: Int -> [String] -> String -> [Int] 
--       findSubstringsHelper _ [] _
--         = []
--       findSubstringsHelper n (suf : sufs) s
--         | isPrefix s suf = n : findSubstringsHelper (n + 1) sufs s
--         | otherwise      = findSubstringsHelper (n + 1) sufs s

-- 3/3
findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = [n | (suf, n) <- zip (suffixes s') [0..], isPrefix s suf]

-- * This is exactly how findIndices is implemented so we can write it as
-- findSubstrings :: String -> String -> [Int]
-- findSubstrings s s'
--   = findIndices (isPrefix s) (suffixes s')

------------------------------------------------------ PART 2: 6.5/8

-- 2/3 Use HOFs
-- getIndices :: SuffixTree -> [Int]
-- getIndices (Leaf i)
--   = [i] 
-- getIndices (Node [])
--   = []
-- getIndices (Node ((s, t) : ps))
--   = (getIndices t) ++ (getIndices (Node ps))

-- * we can use concatMap with snd to make it simpler
getIndices :: SuffixTree -> [Int]
getIndices (Leaf i)
  = [i]
getIndices (Node xs)
  = concatMap (getIndices . snd) xs 

-- 0.5/1 Bad code complexity
-- partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
-- partition s s'
--   = partition' [] s s'
--   where
--     partition' :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
--     partition' com [] s
--       = (reverse com, [], s)
--     partition' com s []
--       = (reverse com, s, [])
--     partition' com w@(c : s) w'@(c' : s')
--       | c == c' = partition' (c : com) s s'
--       | otherwise = (reverse com, w, w')

-- * Use a where clause to get the partition
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition (x : xs) (y : ys)
  | x == y = (x : as, bs, cs)
  where
    (as, bs, cs) = partition xs ys
partition xs ys
  = ([], xs, ys)

-- findSubstrings' :: String -> SuffixTree -> [Int]
-- findSubstrings' s (Node ((a, t) : ps))
--   | isPrefix s a = getIndices (Node [(a, t)])
--   | isPrefix a s = findSubstrings' (removePrefix a s) t
--   | otherwise    = findSubstrings' s (Node ps)
-- findSubstrings' "" (Leaf i)
--   = [i]
-- findSubstrings' s _
--   = []

-- 4/5 + 0.5/1 = 4.5/6
-- findSubstrings' :: String -> SuffixTree -> [Int]
-- findSubstrings' s (Node ((a, t) : ps))
--   | null s'       = getIndices (Node [(a, t)])
--   | null a'       = findSubstrings' s' t
--   | otherwise     = findSubstrings' s (Node ps)
--   where
--     (com, s', a') = partition s a
-- findSubstrings' "" (Leaf i)
--   = [i]
-- findSubstrings' s _
--   = []

-- * We can simplify the above by using _ and not needing Node for the first guard -1
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Node ((a, t) : ps))
  | null s'       = getIndices t
  | null a'       = findSubstrings' s' t
  | otherwise     = findSubstrings' s (Node ps)
  where
    (_, s', a') = partition s a
findSubstrings' "" (Leaf i)
  = [i]
findSubstrings' _ _
  = []
 
  

------------------------------------------------------ PART 3: 2/4

-- 2/4 the 2nd and 3rd lines of guard need to be concatenated with the rest of the tree
-- insert :: (String, Int) -> SuffixTree -> SuffixTree
-- insert (s, n) (Node [])
--   = Node [(s, Leaf n)]
-- insert p@(s, n) (Node ((a, t) : ps))
--   | null com = Node ((a, t) : ps')
--   | com == a = Node ((a, t) : ps'')
--   | otherwise = Node [((com, Node [(s', Leaf n), (a', t)]))]
--   where
--     (com, s', a') = partition s a
--     Node ps' = insert p (Node ps)
--     Node ps'' = insert (s', n) t

-- * The insertion is pushed into t, so we want to concatenate the rest of the tree untouched.
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert p@(s, n) (Node ((a, t) : ps))
  | null com = merge (Node [(a, t)]) (insert p (Node ps)) 
  | com == a = merge (Node [(a, insert (s', n) t)]) (Node ps)
  | otherwise = merge (Node [(com, Node [(s', Leaf n), (a', t)])]) (Node ps)
  where
    (com, s', a') = partition s a
    
-- * We can do this easier using a merge helper function
merge :: SuffixTree -> SuffixTree -> SuffixTree
-- Pre: Arguments must be Nodes
merge (Node xs) (Node ys)
  = Node (xs ++ ys)


-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------ PART 4: 0/1
-- Part IV 0/1 - not attempted

isRepeated :: SuffixTree -> Bool
-- Pre: Argument must be a node
isRepeated (Node (x : xs))
  = True 
isRepeated _
  = False

allRepeatedSubstrings :: SuffixTree -> [String]
allRepeatedSubstrings (Node ((a, t) : ats))
  | isRepeated t = a : (map (a ++ ) (allRepeatedSubstrings t)) ++ allRepeatedSubstrings (Node ats)
  | otherwise    = allRepeatedSubstrings t ++ allRepeatedSubstrings (Node ats)
allRepeatedSubstrings _
  = []

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring t
  = last (sortOn length (allRepeatedSubstrings t))


------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

