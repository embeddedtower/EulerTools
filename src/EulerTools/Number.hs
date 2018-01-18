module EulerTools.Number
( countDivisors
, totient
, totientFromDivs
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

totientFromDivs :: Integral a => a -> [a] -> a
totientFromDivs n divs =
  n * product (map (-1+) divs) `div` product divs
