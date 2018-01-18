module EulerTools.Number
( countDivisors
, totient
) where

import Data.List           (group)
import Data.Numbers.Primes (primeFactors)

countDivisors :: Integral a => a -> Int
countDivisors =
  product . map ((+1) . length) . group . primeFactors

totient :: Integral a => a -> a
totient n = n * product (map (-1+) divs) `div` product divs
  where
    divs = map head . group $ primeFactors n
