module EulerTools.Timer
( timeIO
) where

import System.CPUTime
import Text.Printf

timeIO :: IO () -> IO ()
timeIO f = do
  start <- getCPUTime
  f
  end   <- getCPUTime
  printf "Computation time: %0.3f sec\n" (fromIntegral (end - start) / (10^12) :: Double)
