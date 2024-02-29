module Parser.BodyParser (pBody) where

import RIO
import RIO.Char (isPrint)
import RIO.Set (singleton)
import qualified RIO.Text as T
import qualified Text.Megaparsec as P

import Model.Parser (Parser)
import Model.Dict

pBody :: Parser (DictAttr a)
pBody = do
  t <- Translated <$> pTranslated
  reduce fn t <$> pAttrs
  where
    reduce
      :: (DictAttr a -> (DictAttr a -> DictAttr a) -> DictAttr a) -- fold function
      -> DictAttr a -- initial value
      -> [DictAttr a -> DictAttr a] -- input values to be reduced
      -> DictAttr a
    reduce _ accum [] = accum
    reduce f accum (x:xs) = reduce f (f accum x) xs
    fn :: DictAttr a -> (DictAttr a -> DictAttr a) -> DictAttr a
    fn accum x = x accum

-- TODO remove "◆" if needed
pTranslated :: Parser Text
pTranslated = T.concat <$> p
  where
    pKana :: Parser Text
    pKana = do
      void $ P.single '｛'
      void . P.many $ P.notFollowedBy (P.single '｝') >> pAttrChar []
      void $ P.single '｝'
      pure ""
    pExample :: Parser Text
    pExample = do
      void $ P.chunk "■・"
      void . P.many $ pAttrChar []
      pure ""
    pChar :: Parser Text
    pChar = T.singleton <$> pAttrChar []
    p :: Parser [Text]
    p = P.many $ P.notFollowedBy (P.single '【') >> (P.try pKana <|> P.try pExample <|> pChar)

pAttrs :: Parser [DictAttr a -> DictAttr a]
pAttrs = p
  where
    p :: Parser [DictAttr a -> DictAttr a]
    p = P.many $ pAttr <* (P.try P.eof <|> sep)
    sep = void $ P.single '、'

pAttr :: Parser (DictAttr a -> DictAttr a)
pAttr = do
  header <- pAttrHeader
  body <- pAttrBody
  case header of
    "レベル" ->
      case readMaybe @Natural body of
        Just a -> pure $ Svl a
        Nothing -> invalid header body
    "発音" -> pure . Pron . T.pack $ body
    "発音！" -> pure . Mispron . T.pack $ body
    _ -> pure $ Ignore (header, T.pack body)
  where
    invalid header body = P.fancyFailure
      (singleton (P.ErrorFail ("invalid value of 【" <> T.unpack header <> "】 : " <> body)))

pAttrHeader :: Parser Text
pAttrHeader = do
  void $ P.single '【'
  header <- T.pack <$> P.many (pAttrChar ['】'])
  void $  P.single '】'
  pure header

pAttrBody :: Parser String
pAttrBody = P.many $ pAttrChar ['、']

pAttrChar :: [Char] -> Parser Char
pAttrChar banned = P.satisfy (without banned isAttrChar) P.<?> "attr_char"

without :: [Char] -> (Char -> Bool) -> Char -> Bool
without banned predicate c = notElem c banned && predicate c

isAttrChar :: Char -> Bool
isAttrChar a = isJapanese a || isPrint a

-- 和風の句読点（3000-303f）
-- ひらがな（3040-309f）
-- カタカナ（30a0-30ff）
-- 全角ローマ字および半角カタカナ（ff00-ffef）
-- CJK unifed表意文字-共通および非共通漢字（4e00-9faf）
isJapanese :: Char -> Bool
isJapanese a
  | ('\x3000' <= a) && (a <= '\x303f') = True
  | ('\x3040' <= a) && (a <= '\x309f') = True
  | ('\x30a0' <= a) && (a <= '\x30ff') = True
  | ('\xff00' <= a) && (a <= '\xffef') = True
  | ('\x4e00' <= a) && (a <= '\x9faf') = True
  | otherwise = False
