module EulerTools.String
( isPalindrome
) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome as = as == reverse as
