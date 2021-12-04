module Utils where

import Data.Time.Units as Time

withTime :: IO () -> IO ()
withTime effect = do
  before <- Time.getCPUTimeWithUnit :: IO Time.Microsecond
  effect
  after <- Time.getCPUTimeWithUnit :: IO Time.Microsecond
  print $ (after `Time.subTime` before :: Time.Microsecond)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap = map . toSnd

note :: b -> Maybe a -> Either b a
note b Nothing = Left b
note _ (Just a) = Right a
