module Tries where

import Data.List hiding (insert)
import Data.Bits

import Debug.Trace

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes 0
  = 0
countOnes n
  = countOnes (shiftR n 4) + (bitTable !! (n .&. (bit 4 - 1)))


countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes ((bit i - 1) .&. n)

getIndex :: Int -> Int -> Int -> Int
getIndex n i s
  = (shiftR n (i*s)) .&. (bit s - 1) 

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (_ : xs) v
  = v : xs
replace i (x : xs) v
  = x : (replace (i - 1) xs v)

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 v xs
  = v : xs
insertAt i v (x : xs)
  = x : (insertAt (i - 1) v xs)

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ g (Leaf ns)
  = g ns
sumTrie f g (Node b xs)
  = sumSubNodes xs 
    where
      sumSubNodes :: [SubNode] -> Int
      sumSubNodes []
        = 0
      sumSubNodes ((Term n) : xs)
        = (f n) + (sumSubNodes xs)
      sumSubNodes ((SubTrie t) : xs)
        = (sumTrie f g t) + (sumSubNodes xs)

--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

member :: Int -> Hash -> Trie -> Int -> Bool
member v h t bs
  = member' 0 t
    where
      member' :: Int -> Trie -> Bool
      member' l (Node bv sns)
        = testBit bv i && member''
          where
            i = getIndex h l bs
            n = countOnesFrom i bv
            sn = sns !! n
            member'' :: Bool 
            member''
              | (Term x)    <- sn = v == x
              | (SubTrie t) <- sn = member' (l + 1) t
      member' _ (Leaf xs)
        = elem v xs
      

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf d bs v t
  = insert' 0 v t
    where
      insert' :: Int -> Int -> Trie -> Trie
      insert' l v leaf@(Leaf xs)
        | elem v xs = leaf 
        | otherwise = Leaf (v : xs)
      insert' l v _
        | l == d - 1 = Leaf [v]
      insert' l v (Node bv sns)
        | testBit bv i = Node bv (replace n sns sn')
        | otherwise    = Node (setBit bv i) (insertAt n (Term v) sns)
          where
            i = getIndex (hf v) l bs
            n = countOnesFrom i bv
            sn = sns !! n
            sn' :: SubNode 
            sn' 
              | (SubTrie t) <- sn = SubTrie (insert' (l + 1) v t)
              | (Term v')   <- sn = if v == v' then sn
                                    else SubTrie (insert' (l + 1) (hf v) (insert' (l + 1) (hf v') (Node 0 [])))

      

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf d bs vs
  = foldl (\t v -> insert hf d bs v t) (Node 0 []) vs 