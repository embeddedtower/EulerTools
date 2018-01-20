module EulerTools.Number
( countDivisors
, getDivisors
, totient
, totientFromDivs
, expBySq
, pythagoras2
, pythagoras3
, intRoot
) where

import Data.List           (group, sort)
import Data.Numbers.Primes (primeFactors)

import EulerTools.Digit

-- Prime factorization functions

countDivisors :: Integral a => a -> Int
countDivisors =
  product . map ((+1) . length) . group . primeFactors

mkProducts :: Integral a => [a] -> [a]
mkProducts = scanl (*) 1

listProduct :: Integral a => [a] -> [a] -> [a]
listProduct as bs = (*) <$> as <*> bs

getDivisors :: Integral a => a -> [a]
getDivisors = sort . foldr (listProduct . mkProducts) [1] . group . primeFactors

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

intRoot :: Integral a => a -> a
intRoot 0 = 0
intRoot n
  | (2*r + 1) ^ 2 > n = 2*r
  | otherwise         = 2*r + 1
  where
    r = intRoot $ div n 4
