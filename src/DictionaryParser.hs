module DictionaryParser (
  runDictionaryParser,
  tshowParseErrorBundle,
) where

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import Parser (Parser)
import HeaderWordParser (pWord, pDelim)
import Dict (DictHeader(..), DictAttr(..))

runDictionaryParser :: Text -> Either (P.ParseErrorBundle Text Void) (DictHeader a, DictAttr a)
runDictionaryParser = P.runParser parser "DictionaryParser"

tshowParseErrorBundle :: P.ParseErrorBundle Text Void -> Text
tshowParseErrorBundle = T.pack . P.errorBundlePretty

parser :: Parser (DictHeader a, DictAttr a)
parser = do
  pEntryMark
  w <- T.concat <$> pWord
  pDelim
  void P.takeRest
  P.eof
  pure (Word w, Translated "ふう")

pEntryMark :: Parser ()
pEntryMark = void $ P.single '■'
