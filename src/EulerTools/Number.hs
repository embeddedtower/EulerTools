module EulerTools.Number
( countDivisors
, totient
, totientFromDivs
, expBySq
, pythagoras2
, pythagoras3
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

-- Pythagorean triples
-- Coprime for m>n, m,n coprime, at least one of m,n even

pythagoras2 :: Integral a => a -> a -> (a,a)
pythagoras2 m n = (m^2 - n^2, 2*m*n)

pythagoras3 :: Integral a => a -> a -> (a,a,a)
pythagoras3 m n = (m^2 - n^2, 2*m*n, m^2 + n^2)
