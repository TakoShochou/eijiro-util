module Model.Parser (Parser) where

import RIO
import qualified Text.Megaparsec as P

type Parser a = P.Parsec Void Text a
