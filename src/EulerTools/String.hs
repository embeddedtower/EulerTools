module EulerTools.String
( isPalindrome
, mkPalindromes
) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome as = as == reverse as

mkPalindromes :: Int -> Int -> [[Int]]
mkPalindromes b n
  | n == 0    = [[]]
  | n == 1    = [ [k] | k <- [0..b-1] ]
  | otherwise = [ k:p ++ [k] | k <- [0..b-1] , p <- mkPalindromes b (n-2) ]
