module Utils.Parsing where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P

spaces :: Parser ()
spaces = P.skipMany P.space
