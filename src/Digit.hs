module Digit
( getBaseDigits
) where

getBaseDigits :: Integral a => a -> a -> [a]
getDecDigits b n
  | n < b    = [n]
  | otherwise = mod n b : getDecDigits (div n b)
