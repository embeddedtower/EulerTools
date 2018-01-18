module EulerTools.Digit
( getBaseDigits
) where

getBaseDigits :: Integral a => a -> a -> [a]
getBaseDigits b n
  | n < b    = [n]
  | otherwise = mod n b : getBaseDigits b (div n b)
