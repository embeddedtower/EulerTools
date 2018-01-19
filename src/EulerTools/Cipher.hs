module EulerTools.Cipher
( getAlphaValue
, toAlphaLower
, toAlphaUpper
) where

import Data.Char (ord, chr, isUpper, isLetter)

getAlphaValue :: Char -> Int
getAlphaValue c
  | not $ isLetter c = error "can't convert non-letter"
  | isUpper c = ord c - 64
  | otherwise = ord c - 96

toAlphaLower :: Int -> Char
toAlphaLower n
  | n < 1 || n > 26 = error "value out of range"
  | otherwise = chr $ n + 96

toAlphaUpper :: Int -> Char
toAlphaUpper n
  | n < 1 || n > 26 = error "value out of range"
  | otherwise = chr $ n + 64

words' :: String -> [String]
words' [] = []
words' str@(c:_)
  | isLetter c = takeWhile isLetter str : words' (dropWhile isLetter str)
  | otherwise  = words' $ dropWhile (not . isLetter) str
