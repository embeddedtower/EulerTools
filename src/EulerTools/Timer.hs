module EulerTools.Timer
( timeIO
) where

import System.CPUTime

timeIO :: IO () -> IO Double
timeIO f = do
  start <- getCPUTime
  f
  end   <- getCPUTime
  return $ fromIntegral (end - start) / (10^12)
