{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (guard, void)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text, splitOn)
import qualified Data.Text.IO as TextIO
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

data FieldLabel
  = Byr -- Birth Year
  | Iyr -- Issue Year
  | Eyr -- Expiration Year
  | Hgt -- Height
  | Hcl -- Hair Color
  | Ecl -- Eye Color
  | Pid -- Passport ID
  | Cid -- Country ID
  deriving (Eq, Ord, Show)

mkLabel :: String -> FieldLabel
mkLabel "byr" = Byr
mkLabel "iyr" = Iyr
mkLabel "eyr" = Eyr
mkLabel "hgt" = Hgt
mkLabel "hcl" = Hcl
mkLabel "ecl" = Ecl
mkLabel "pid" = Pid
mkLabel "cid" = Cid
mkLabel s = error $ "Invalid field label '" <> s <> "'"

field :: Parser FieldLabel
field = do
  key <- mkLabel <$> P.choice (fmap (P.try . P.string) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"])
  void $ P.char ':'
  void $ P.many $ P.noneOf "\n "
  pure key

passport :: Parser [FieldLabel]
passport = P.endBy1 field (void P.space <|> void P.newline <|> P.eof)

hasEnoughFields :: [FieldLabel] -> Bool
hasEnoughFields pspt =
  length pspt == 8 || (length pspt == 7 && notElem Cid pspt)

data Height
  = Cm Int
  | In Int
  deriving (Eq, Show)

data Field
  = BYR Int
  | IYR Int
  | EYR Int
  | HGT Height
  | HCL String
  | ECL String
  | PID String
  | CID String
  deriving (Show)

instance Eq Field where
  (BYR _) == (BYR _) = True
  (IYR _) == (IYR _) = True
  (EYR _) == (EYR _) = True
  (HGT _) == (HGT _) = True
  (HCL _) == (HCL _) = True
  (ECL _) == (ECL _) = True
  (PID _) == (PID _) = True
  (CID _) == (CID _) = True
  _ == _ = False

int :: Parser Int
int = read <$> P.many P.digit

fieldP :: Parser Field
fieldP = do
  fieldLabel <- P.count 3 P.letter <* P.char ':'
  field <- case fieldLabel of
    "byr" -> do
      n <- int
      guard (n >= 1920 && n <= 2002)
      pure $ BYR n
    "iyr" -> do
      n <- int
      guard (n >= 2010 && n <= 2020)
      pure $ IYR n
    "eyr" -> do
      n <- int
      guard (n >= 2020 && n <= 2030)
      pure $ EYR n
    "hgt" -> do
      n <- int
      unit <- P.count 2 P.letter
      case unit of
        "cm" -> do
          guard (n >= 150 && n <= 193)
          pure $ HGT $ Cm n
        "in" -> do
          guard (n >= 59 && n <= 76)
          pure $ HGT $ In n
    "hcl" -> do
      void $ P.char '#'
      HCL <$> P.count 6 (P.oneOf $ ['0' .. '9'] ++ ['a' .. 'f'])
    "ecl" -> do
      ecl <- P.count 3 P.letter
      guard (ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
      pure $ ECL ecl
    "pid" -> PID <$> P.count 9 P.digit
    "cid" -> CID <$> P.many P.digit
  P.spaces
  pure field

(<&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&>) = liftA2 (&&)

hasAllRequiredFields :: [Field] -> Bool
hasAllRequiredFields =
  (BYR 0 `elem`)
    <&> (IYR 0 `elem`)
    <&> (EYR 0 `elem`)
    <&> (HGT (Cm 0) `elem`)
    <&> (HCL "" `elem`)
    <&> (ECL "" `elem`)
    <&> (PID "" `elem`)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

parseValidPassports :: Text -> [[Field]]
parseValidPassports input =
  input
    & splitOn "\n\n"
    & map (P.parse (P.many fieldP) "passportP")
    & mapMaybe hush
    & filter hasAllRequiredFields

day04 :: IO ()
day04 = do
  input <- TextIO.readFile "input/day04.txt"
  let (Right passports) = P.parse (P.sepBy passport P.space) "input/day04.txt" input
  putStrLn $ "Part 01: " <> show (length $ filter hasEnoughFields passports) <> " passports with 7 or 8 fields"
  putStrLn $ "Part 02: " <> show (length $ parseValidPassports input) <> " valid passports"
