module Parser.DictionaryParser (
  ParserResult,
  runDictionaryParser,
  tshowParseErrorBundle,
) where

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import Parser.HeaderParser (pHeader)
import Parser.BodyParser (pBody)
import Model.Parser (Parser)
import Model.Dict (DictEntry, DictHeader(..), DictAttr(..))

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
