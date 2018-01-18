module EulerTools.Number
( countDivisors
, totient
, totientFromDivs
, expBySq
) where

import Data.List           (group)
import Data.Numbers.Primes (primeFactors)

import EulerTools.Digit

-- Prime factorization functions

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

-- Fast modular exponentiation by squaring

expBySq :: Integral a => a -> a -> a -> a
expBySq modulus base =
  foldr (\a b -> mod (a*b) modulus) 1 . ebsWorker base . getBaseDigits 2
  where
    square n = mod (n*n) modulus
    ebsWorker _ []     = []
    ebsWorker n (b:bs) | b == 1    = n : ebsWorker (square n) bs
                       | otherwise = 1 : ebsWorker (square n) bs
