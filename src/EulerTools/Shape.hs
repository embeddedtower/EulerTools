module EulerTools.Shape
( intRoot
, triangle
, isTriangle
, pentagon
, isPentagon
, hexagon
, isHexagon
) where

intRoot :: Integral a => a -> a
intRoot = floor . sqrt . fromIntegral

triangle :: Integral a => a -> a
triangle n = n * (n+1) `div` 2

isTriangle :: Integral a => a -> Bool
isTriangle n = n == k * (k+1) `div` 2
  where
    k = intRoot (2*n)

pentagon :: Integral a => a -> a
pentagon n = n * (3*n - 1) `div` 2

isPentagon :: Int -> Bool
isPentagon n = n == k * (3*k - 1) `div` 2
  where
    k = 1 + intRoot (2*n `div` 3)

hexagon :: Integral a => a -> a
hexagon n = n * (2*n - 1)

isHexagon :: Int -> Bool
isHexagon n = n == k * (2*k - 1)
  where
    k = 1 + intRoot (n `div` 2)
