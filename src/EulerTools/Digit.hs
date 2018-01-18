module EulerTools.Digit
( getBaseDigits
, fromBaseDigits
) where

getBaseDigits :: Integral a => a -> a -> [a]
getBaseDigits b n
  | n < b    = [n]
  | otherwise = mod n b : getBaseDigits b (div n b)

fromBaseDigits :: Integral a => a -> [a] -> a
fromBaseDigits b []     = 0
fromBaseDigits b (d:ds) = d + b * fromBaseDigits b ds
