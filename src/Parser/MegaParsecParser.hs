module Parser.MegaParsecParser (MegaParsecParser) where

import RIO
import qualified Text.Megaparsec as MP

type MegaParsecParser a = MP.Parsec Void Text a
