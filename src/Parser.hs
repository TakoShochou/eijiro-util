module Parser (Parser) where

import RIO
import RIO.Text
import qualified Text.Megaparsec as P

type Parser = P.Parsec Void Text
