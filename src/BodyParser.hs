module BodyParser (pTranslated, pAttrs) where

import RIO
import RIO.Char (isPrint)
import RIO.Set (singleton)
import qualified RIO.Text as T
import qualified Text.Megaparsec as P

import Parser (Parser)
import Dict

pTranslated :: Parser Text
pTranslated = do
  T.pack <$> P.many (P.notFollowedBy (P.single '【') >> P.try (P.satisfy isJapanese))

pAttrs :: Parser ([DictAttr a])
pAttrs = P.many pAttr -- <* (P.try (void (P.single '、')) <|> void P.eof)

pAttr :: Parser (DictAttr a)
pAttr = do
  header <- pAttrHeader
  pure $ Svl 12
  {-
  body <- T.pack <$> P.many pAttrChar
  case header of
    "レベル" -> Svl <$> case readMaybe @(Maybe Natural) body of
      Nothing -> P.fancyFailure (singleton (P.ErrorFail "invalid value of レベル attr"))
      Just a -> a
    "発音" -> Pron <$> body
    "発音！" -> case Mispron $ readMaybe @(Maybe Bool) body of
      Nothing -> P.fancyFailure (singleton (P.ErrorFail "invalid value of 発音！"))
      Just a -> Mispron <$> a
    _ -> P.fancyFailure (singleton (P.ErrorFail "invalid attr header. expect: レベル 発音 発音！"))
  -}

pAttrHeader :: Parser Text
pAttrHeader = do
  void $ P.single '【'
  header <- T.pack <$> P.many pAttrChar <* P.notFollowedBy (P.single '】')
  void $ P.single '】'
  pure header

-- 和風の句読点（3000-303f）
-- ひらがな（3040-309f）
-- カタカナ（30a0-30ff）
-- 全角ローマ字および半角カタカナ（ff00-ffef）
-- CJK unifed表意文字-共通および非共通漢字（4e00-9faf）
pAttrChar :: Parser Char
pAttrChar = P.satisfy isAttrChar P.<?> "attr_char"

isAttrChar :: Char -> Bool
isAttrChar a = isJapanese a || isPrint a

isJapanese :: Char -> Bool
isJapanese a
  | ('\x3000' <= a) && (a <= '\x303f') = True
  | ('\x3040' <= a) && (a <= '\x309f') = True
  | ('\x30a0' <= a) && (a <= '\x30ff') = True
  | ('\xff00' <= a) && (a <= '\xffef') = True
  | ('\x4e00' <= a) && (a <= '\x9faf') = True
  | otherwise = False
