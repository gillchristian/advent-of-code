module Utils where

import Data.Time.Units as Time

withTime :: IO () -> IO ()
withTime effect = do
  before <- Time.getCPUTimeWithUnit :: IO Time.Microsecond

  effect

  after <- Time.getCPUTimeWithUnit :: IO Time.Microsecond

  print $ (after `Time.subTime` before :: Time.Microsecond)
