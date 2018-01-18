module EulerTools.Number
( countDivisors
) where

import Data.List           (group)
import Data.Numbers.Primes (primeFactors)

countDivisors :: Integral a => a -> Int
countDivisors =
  product . map ((+1) . length) . group . primeFactors
