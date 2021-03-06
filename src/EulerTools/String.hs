module EulerTools.String
( isPermOf
, isNub
, rotateN
, isPalindrome
, mkPalindromes
, lexN
, words'
, ints
) where

import           Data.Char (isDigit, isLetter)
import           Data.List (nub, sort)

isPermOf :: Ord a => [a] -> [a] -> Bool
isPermOf as bs = sort as == sort bs

isNub :: Eq a => [a] -> Bool
isNub l = isNub' l []
  where
    isNub' [] _      = True
    isNub' (y:ys) xs
       | y `elem` xs = False
       | otherwise   = isNub' ys (y:xs)

rotateN :: [a] -> Int -> [a]
rotateN as n = drop n' as ++ take n' as
  where
    n' = mod n $ length as

isPalindrome :: Eq a => [a] -> Bool
isPalindrome as = as == reverse as

mkPalindromes :: Int -> Int -> [[Int]]
mkPalindromes b n
  | n == 0    = [[]]
  | n == 1    = [ [k] | k <- [0..b-1] ]
  | otherwise = [ k:p ++ [k] | k <- [0..b-1] , p <- mkPalindromes b (n-2) ]

dropAt :: Int -> [a] -> [a]
dropAt n as = take n as ++ drop (n+1) as

fac :: Integral a => a -> a
fac n = product [1..n]

lexN :: Int -> [a] -> [a]
lexN _ [] = []
lexN n ds = (ds !! i) : lexN n' (dropAt i ds)
    where
      a  = fac $ length ds - 1
      i  = div n a
      n' = mod n a

words' :: String -> [String]
words' [] = []
words' str@(c:_)
  | isLetter c = takeWhile isLetter str : words' (dropWhile isLetter str)
  | otherwise  = words' $ dropWhile (not . isLetter) str

isNumeric :: Char -> Bool
isNumeric c = c == '-' || isDigit c

ints :: String -> [Int]
ints str = map read $ go str
  where
    go [] = []
    go (c:cs)
      | isNumeric c = (c : takeWhile isDigit cs) : go (dropWhile isDigit cs)
      | otherwise   = go $ dropWhile (not . isNumeric) cs
