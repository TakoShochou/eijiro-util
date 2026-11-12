module Parser.MegaParsecHeaderParser (pHeader) where

import qualified Model.Dict as D
import Model.HeadWordTerminals(headWordTerminals)

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as PCL
import Parser.MegaParsecParser (MegaParsecParser)
type Parser a = MegaParsecParser a

pHeader :: Parser (D.DictHeader a)
pHeader = do
  pEntryMark
  w <- pWord
  l <- optional pHeaderLabel
  pDelim
  case l of
    Just l' -> pure $ l' w
    Nothing -> pure w

pEntryMark :: Parser ()
pEntryMark = void $ P.single '■'

pDelim :: Parser ()
pDelim = void $ P.chunk " : "

pHeaderLabel :: Parser (D.DictHeader a -> D.DictHeader a)
pHeaderLabel = P.try pIndex <|> P.try pIndexLabel <|> P.try pIndexLabelIndex
  <|> P.try pLabel <|> P.try pLabelIndex <|> pLabelIndexIndex

-- {123}
pIndex :: Parser (D.DictHeader a -> D.DictHeader a)
pIndex = do
  void $ P.single '{'
  index <- PCL.decimal
  void $ P.single '}'
  pure $ D.Index index

-- {1-ラベル}
pIndexLabel :: Parser (D.DictHeader a -> D.DictHeader a)
pIndexLabel = do
  void $ P.single '{'
  index <- PCL.decimal
  void $ P.single '-'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '}'
  pure $ D.Index index <$> D.Label label

-- {123-ラベル-456}
pIndexLabelIndex :: Parser (D.DictHeader a -> D.DictHeader a)
pIndexLabelIndex = do
  void $ P.single '{'
  index1 <- PCL.decimal
  void $ P.single '-'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '-'
  index2 <- PCL.decimal
  void $ P.single '}'
  pure $ D.LabelIndex index2 . D.Label label <$> D.Index index1

-- {ラベル}
pLabel :: Parser (D.DictHeader a -> D.DictHeader a)
pLabel = do
  void $ P.single '{'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '}'
  pure $ D.Label label

-- {ラベル-123}
pLabelIndex :: Parser (D.DictHeader a -> D.DictHeader a)
pLabelIndex = do
  void $ P.single '{'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '-'
  index <- PCL.decimal
  void $ P.single '}'
  pure $ D.LabelIndex index <$> D.Label label

-- {ラベル-123-456}
pLabelIndexIndex :: Parser  (D.DictHeader a -> D.DictHeader a)
pLabelIndexIndex = do
  void $ P.single '{'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '-'
  index1 <- PCL.decimal
  void $ P.single '-'
  index2 <- PCL.decimal
  void $ P.single '}'
  pure $ D.LabelIndexIndex index2 . D.LabelIndex index1 <$> D.Label label

{-
手許のデータでは1か所だけ見出し語に`;`が含まれている行があるが、現時点ではSVLに関係しないのでpWordのこの構造
は取り込まない。
■Stephen  {人名-2} ; Steven  {人名}
-}
pWord :: Parser (D.DictHeader a)
pWord = D.Word . T.strip <$> p4
  where
    p4 :: Parser Text
    p4 = T.concat <$> p3
    p3 :: Parser [Text]
    p3 = P.some (p2 >> pText)
    p2 :: Parser ()
    p2 = P.notFollowedBy pDelim

pText :: Parser Text
pText = P.choice $ singleText <$> headWordTerminals
  where
    singleText :: Char -> Parser Text
    singleText c = T.singleton <$> P.single c
