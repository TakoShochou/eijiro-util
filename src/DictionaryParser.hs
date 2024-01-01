module DictionaryParser (
  runDictionaryParser,
  tshowParseErrorBundle,
) where

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import Parser (Parser)
import HeaderParser (pHeader)
import BodyParser (pTranslated, pAttrs)
import Dict (DictHeader(..), DictAttr(..))

runDictionaryParser :: Text -> Either (P.ParseErrorBundle Text Void) (DictHeader Text, [DictAttr Text])
runDictionaryParser = P.runParser parser "DictionaryParser"

tshowParseErrorBundle :: P.ParseErrorBundle Text Void -> Text
tshowParseErrorBundle = T.pack . P.errorBundlePretty

parser :: Parser (DictHeader a, [DictAttr a])
parser = do
  header <- pHeader
  translated <- pTranslated
  attrs <- P.try pAttrs
  void P.takeRest
  P.eof
  pure (header, Translated translated : attrs)
