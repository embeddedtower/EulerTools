{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module EulerTools.BFS
( Alphabet(..)
, Recent(..)
, Solution(..)
, Soluble(..)
, branchN
, branchUntil
, total
, totalWith
, allSolvedN
, lastSolvedN
, Forgettable(..)
, Forgetful(..)
, Countable(..)
, CountsOne(..)
, Memorable(..)
, Heedful(..)
) where

import qualified Data.Map.Strict  as M
import           Data.Maybe       (mapMaybe)
import           EulerTools.Timer

-- Combinatorial objects which can be extended based on only a small "recent"
-- component; e.g. lists of symbols 'a','b','c' with no more than three
-- consecutive symbols equal to one another

class Ord a => Alphabet a where
  alphabet :: [a]

class (Alphabet a, Ord (t a)) => Recent t a where
  update   :: t a -> a -> Maybe (t a)
  initial  :: Integral n => M.Map (t a) n

-- Make all possible new objects based on one starting object

branch :: (Recent t a, Integral n) => t a -> n -> M.Map (t a) n
branch as n = M.fromListWith (+) ((,n) <$> mapMaybe (update as) alphabet)

-- Update a map of counts of equivalent objects with one or more stages of branching

branchAll :: (Recent t a, Integral n) => M.Map (t a) n -> M.Map (t a) n
branchAll = M.unionsWith (+) . M.foldrWithKey (\ta n ms -> branch ta n : ms) []

branchN :: (Recent t a, Integral n) => Int -> M.Map (t a) n -> M.Map (t a) n
branchN n mta
  | n <= 0    = mta
  | otherwise = branchN (n-1) $ branchAll mta

-- Count all objects, with or without adjusting totals

total :: (Recent t a, Integral n) => M.Map (t a) n -> n
total = M.foldr (+) 0

totalWith :: (Recent t a, Integral n) => (t a -> n -> n) -> M.Map (t a) n -> n
totalWith f = M.foldrWithKey (\ta n b -> b + f ta n) 0

-- Combinatorial objects with a solved state, e.g a bridge of a certain length
-- made out of various potential components

data Solution a b = Solved a b | Continue

instance Monoid (Solution a b) where
  mempty                 = Continue
  mappend Continue     s = s
  mappend (Solved a b) _ = Solved a b

class Recent t a => Soluble t a where
  isSolved :: Integral n => t a -> n -> Bool

solved :: (Soluble t a, Integral n) => t a -> n -> Solution (t a) n
solved ta n = if isSolved ta n
  then Solved ta n
  else Continue

eitherSolved :: (Soluble t a, Integral n) => t a -> n -> Either n n
eitherSolved ta n = if isSolved ta n
  then Left  n
  else Right n

-- Check whether a map contains a solved object

searchSolution :: (Soluble t a, Integral n) => M.Map (t a) n -> Solution (t a) n
searchSolution = M.foldMapWithKey solved

-- Branch until a solved object is found

branchSolution :: (Soluble t a, Integral n) => M.Map (t a) n -> Solution (t a) n
branchSolution mta = case searchSolution mta of
  Solved ta n -> Solved ta n
  Continue    -> branchSolution $ branchAll mta

branchUntil :: (Soluble t a, Integral n) => M.Map (t a) n -> M.Map (t a) n
branchUntil mta = case searchSolution mta of
  Solved _ _ -> mta
  Continue   -> branchUntil $ branchAll mta

-- Branch n times, collecting all solved objects

splitSolved :: (Soluble t a, Integral n) => M.Map (t a) n -> (M.Map (t a) n, M.Map (t a) n)
splitSolved = M.mapEitherWithKey eitherSolved

allSolvedN :: (Soluble t a, Integral n) => Int -> M.Map (t a) n -> M.Map (t a) n
allSolvedN n mta
  | n <= 0    = fst splits
  | otherwise = M.unionWith (+) (fst splits) $ allSolvedN (n-1) (branchAll $ snd splits)
  where
    splits = splitSolved mta

lastSolvedN :: (Soluble t a, Integral n) => Int -> M.Map (t a) n -> M.Map (t a) n
lastSolvedN n mta =
  fst . splitSolved $ branchN n mta


-- 'Recent' object with no memory other than the the last n symbols

class Alphabet a => Forgettable a where
  memoryF :: a -> Int
  decideF :: a -> [a] -> Bool

newtype Forgetful a = Forgetful [a] deriving (Eq, Ord, Show)

instance Forgettable a => Recent Forgetful a where
  update :: Forgetful a -> a -> Maybe (Forgetful a)
  update (Forgetful as) a =
    if decideF a as
      then Just . Forgetful $ take (memoryF a) (a:as)
      else Nothing

  initial :: Integral n => M.Map (Forgetful a) n
  initial = M.singleton (Forgetful []) 1

-- 'Recent' object with one counter

class Alphabet a => Countable a where
  memoryC :: a -> Int
  countC  :: a
  decideC :: Int -> a -> [a] -> Bool

data CountsOne a = CountOne Int [a] deriving (Eq, Ord, Show)

instance Countable a => Recent CountsOne a where
  update :: CountsOne a -> a -> Maybe (CountsOne a)
  update (CountOne m as) a =
    if decideC m a as
      then Just . CountOne m' $ take (memoryC a) (a:as)
      else Nothing
    where
      m' = if a == countC then m+1 else m

  initial :: Integral n => M.Map (CountsOne a) n
  initial = M.singleton (CountOne 0 []) 1

data Test2 = A | L | O deriving (Eq, Show, Ord)

instance Alphabet Test2 where
  alphabet = [A,L,O]

instance Countable Test2 where
  countC  = L
  memoryC = const 2
  decideC 1 L _       = False
  decideC _ A (A:A:_) = False
  decideC _ _ _       = True

solve2 :: Int
solve2 = total $ branchN 30 (initial :: M.Map (CountsOne Test2) Int)

type Test3 = Int

instance Alphabet Test3 where
  alphabet = [0..9]

instance Forgettable Test3 where
  memoryF = const 2
  decideF n ns = sum (n : ns) <= 9

solve3 :: Int
solve3 = totalWith (\(Forgetful as) n -> if head as == 0 then 0 else n) $
  branchN 20 (initial :: M.Map (Forgetful Test3) Int)

-- 'Recent' object which uses each alphabet symbol at most once

class Alphabet a => Memorable a where
  memoryM :: a -> Int
  decideM :: a -> [a] -> Bool

data Heedful a = Heedful [a] [a] deriving (Eq, Show, Ord)

instance Memorable a => Recent Heedful a where
  update :: Heedful a -> a -> Maybe (Heedful a)
  update (Heedful bs as) a =
    if decideM a as && a `notElem` bs
      then Just . Heedful (a:bs) $ take (memoryM a) (a:as)
      else Nothing

  initial :: Integral n => M.Map (Heedful a) n
  initial = M.singleton (Heedful [] []) 1

newtype Test4 = T4 Int deriving (Eq, Show, Ord)

instance Alphabet Test4 where
  alphabet = T4 <$> [0..9]

instance Memorable Test4 where
  memoryM     = const 0
  decideM _ _ = True

solve4 :: Int
solve4 = total $ branchN 5 (initial :: M.Map (Heedful Test4) Int)

newtype Test5 = T5 Int deriving (Eq, Show, Ord)

instance Alphabet Test5 where
  alphabet = T5 <$> [0..7]

instance Forgettable Test5 where
  memoryF                 = const 1
  decideF _ []            = True
  decideF (T5 n) (T5 m:_) = n >= m

instance Soluble Forgetful Test5 where
  isSolved (Forgetful [])     _ = False
  isSolved (Forgetful (n:ns)) _ = n == T5 7

solve5, solve6 :: Int
solve5 = total $ allSolvedN  50 (initial :: M.Map (Forgetful Test5) Int)
solve6 = total $ lastSolvedN 50 (initial :: M.Map (Forgetful Test5) Int)

main :: IO ()
main = do
  timeIO $ print solve2
  timeIO $ print solve3
  timeIO $ print solve4
  timeIO $ print solve5
  timeIO $ print solve6
