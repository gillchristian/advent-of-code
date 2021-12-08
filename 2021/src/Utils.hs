module Utils where

import Data.List (find)
import Data.Maybe (fromMaybe)
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

loopN :: Int -> (a -> a) -> a -> a
loopN 0 _ a = a
loopN n f a = loopN (n - 1) f (f a)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) f g = (&&) <$> f <*> g

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) f g = (||) <$> f <*> g

toList4 :: (a, a, a, a) -> [a]
toList4 (a, b, c, d) = [a, b, c, d]

unsafeFind :: (a -> Bool) -> [a] -> a
unsafeFind f = fromMaybe (error "Found nothing xD") . find f
