module Parser.MegaParsecDictionaryParser (
  ParserResult,
  runDictionaryParser,
  tshowParseErrorBundle,
) where

import Model.Dict (DictEntry, DictHeader(..), DictAttr(..))

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import Parser.MegaParsecHeaderParser (pHeader)
import Parser.MegaParsecBodyParser (pBody)
import Parser.MegaParsecParser (MegaParsecParser)
type Parser a = MegaParsecParser a

type ParserResult = Either
  (P.ParseErrorBundle Text Void)
  DictEntry

runDictionaryParser :: Text -> Either (P.ParseErrorBundle Text Void) (DictHeader Text, DictAttr Text)
runDictionaryParser = P.runParser parser "MegaParsecDictionaryParser"

tshowParseErrorBundle :: P.ParseErrorBundle Text Void -> Text
tshowParseErrorBundle = T.pack . P.errorBundlePretty

parser :: Parser (DictHeader a, DictAttr a)
parser = do
  header <- pHeader
  body <- pBody
  void P.takeRest
  P.eof
  pure (header, body)
