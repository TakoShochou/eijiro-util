module DictionaryParser (
  ParserResult,
  DictEntry,
  runDictionaryParser,
  tshowParseErrorBundle,
) where

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import Parser (Parser)
import HeaderParser (pHeader)
import BodyParser (pBody)
import Dict (DictHeader(..), DictAttr(..))

type DictEntry = (DictHeader Text, DictAttr Text)
type ParserResult = Either
  (P.ParseErrorBundle Text Void)
  DictEntry

runDictionaryParser :: Text -> Either (P.ParseErrorBundle Text Void) (DictHeader Text, DictAttr Text)
runDictionaryParser = P.runParser parser "DictionaryParser"

tshowParseErrorBundle :: P.ParseErrorBundle Text Void -> Text
tshowParseErrorBundle = T.pack . P.errorBundlePretty

parser :: Parser (DictHeader a, DictAttr a)
parser = do
  header <- pHeader
  body <- pBody
  void P.takeRest
  P.eof
  pure (header, body)
