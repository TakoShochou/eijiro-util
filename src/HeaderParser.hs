module HeaderParser (pHeader) where

import RIO
import qualified RIO.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as PCL
import Parser (Parser)
import Dict

pHeader :: Parser (DictHeader a)
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

pHeaderLabel :: Parser (DictHeader a -> DictHeader a)
pHeaderLabel = P.try pIndex <|> P.try pIndexLabel <|> P.try pIndexLabelIndex
  <|> P.try pLabel <|> P.try pLabelIndex <|> pLabelIndexIndex

-- {123}
pIndex :: Parser (DictHeader a -> DictHeader a)
pIndex = do
  void $ P.single '{'
  index <- PCL.decimal
  void $ P.single '}'
  pure $ Index index

-- {1-ラベル}
pIndexLabel :: Parser (DictHeader a -> DictHeader a)
pIndexLabel = do
  void $ P.single '{'
  index <- PCL.decimal
  void $ P.single '-'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '}'
  pure $ Index index <$> Label label

-- {123-ラベル-456}
pIndexLabelIndex :: Parser (DictHeader a -> DictHeader a)
pIndexLabelIndex = do
  void $ P.single '{'
  index1 <- PCL.decimal
  void $ P.single '-'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '-'
  index2 <- PCL.decimal
  void $ P.single '}'
  pure $ LabelIndex index2 . Label label <$> Index index1

-- {ラベル}
pLabel :: Parser (DictHeader a -> DictHeader a)
pLabel = do
  void $ P.single '{'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '}'
  pure $ Label label

-- {ラベル-123}
pLabelIndex :: Parser (DictHeader a -> DictHeader a)
pLabelIndex = do
  void $ P.single '{'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '-'
  index <- PCL.decimal
  void $ P.single '}'
  pure $ LabelIndex index <$> Label label

-- {ラベル-123-456}
pLabelIndexIndex :: Parser  (DictHeader a -> DictHeader a)
pLabelIndexIndex = do
  void $ P.single '{'
  label <- T.concat <$> P.some (P.notFollowedBy (P.single '-') >> pText)
  void $ P.single '-'
  index1 <- PCL.decimal
  void $ P.single '-'
  index2 <- PCL.decimal
  void $ P.single '}'
  pure $ LabelIndexIndex index2 . LabelIndex index1 <$> Label label

pWord :: Parser (DictHeader a)
pWord = Word <$> p4
  where
    p4 :: Parser Text
    p4 = T.concat <$> p3
    p3 :: Parser [Text]
    p3 = P.some (p2 >> pText)
    p2 :: Parser ()
    p2 = P.notFollowedBy pDelim

pText :: Parser Text
pText = P.choice
  [ T.singleton <$> P.single ' ' -- U+0020
  , T.singleton <$> P.single '!' -- U+0021
  , T.singleton <$> P.single '"' -- U+0022
  , T.singleton <$> P.single '#' -- U+0023
  , T.singleton <$> P.single '$' -- U+0024
  , T.singleton <$> P.single '%' -- U+0025
  , T.singleton <$> P.single '&' -- U+0026
  , T.singleton <$> P.single '\'' -- U+0027
  , T.singleton <$> P.single '(' -- U+0028
  , T.singleton <$> P.single ')' -- U+0029
  , T.singleton <$> P.single '*' -- U+002a
  , T.singleton <$> P.single '+' -- U+002b
  , T.singleton <$> P.single ',' -- U+002c
  , T.singleton <$> P.single '-' -- U+002d
  , T.singleton <$> P.single '.' -- U+002e
  , T.singleton <$> P.single '/' -- U+002f
  , T.singleton <$> P.single '0' -- U+0030
  , T.singleton <$> P.single '1' -- U+0031
  , T.singleton <$> P.single '2' -- U+0032
  , T.singleton <$> P.single '3' -- U+0033
  , T.singleton <$> P.single '4' -- U+0034
  , T.singleton <$> P.single '5' -- U+0035
  , T.singleton <$> P.single '6' -- U+0036
  , T.singleton <$> P.single '7' -- U+0037
  , T.singleton <$> P.single '8' -- U+0038
  , T.singleton <$> P.single '9' -- U+0039
  , T.singleton <$> P.single ':' -- U+003a
  , T.singleton <$> P.single ';' -- U+003b
  , T.singleton <$> P.single '=' -- U+003d
  , T.singleton <$> P.single '?' -- U+003f
  , T.singleton <$> P.single 'A' -- U+0041
  , T.singleton <$> P.single 'B' -- U+0042
  , T.singleton <$> P.single 'C' -- U+0043
  , T.singleton <$> P.single 'D' -- U+0044
  , T.singleton <$> P.single 'E' -- U+0045
  , T.singleton <$> P.single 'F' -- U+0046
  , T.singleton <$> P.single 'G' -- U+0047
  , T.singleton <$> P.single 'H' -- U+0048
  , T.singleton <$> P.single 'I' -- U+0049
  , T.singleton <$> P.single 'J' -- U+004a
  , T.singleton <$> P.single 'K' -- U+004b
  , T.singleton <$> P.single 'L' -- U+004c
  , T.singleton <$> P.single 'M' -- U+004d
  , T.singleton <$> P.single 'N' -- U+004e
  , T.singleton <$> P.single 'O' -- U+004f
  , T.singleton <$> P.single 'P' -- U+0050
  , T.singleton <$> P.single 'Q' -- U+0051
  , T.singleton <$> P.single 'R' -- U+0052
  , T.singleton <$> P.single 'S' -- U+0053
  , T.singleton <$> P.single 'T' -- U+0054
  , T.singleton <$> P.single 'U' -- U+0055
  , T.singleton <$> P.single 'V' -- U+0056
  , T.singleton <$> P.single 'W' -- U+0057
  , T.singleton <$> P.single 'X' -- U+0058
  , T.singleton <$> P.single 'Y' -- U+0059
  , T.singleton <$> P.single 'Z' -- U+005a
  , T.singleton <$> P.single '[' -- U+005b
  , T.singleton <$> P.single '\\' -- U+005c
  , T.singleton <$> P.single ']' -- U+005d
  , T.singleton <$> P.single '_' -- U+005f
  , T.singleton <$> P.single '`' -- U+0060
  , T.singleton <$> P.single 'a' -- U+0061
  , T.singleton <$> P.single 'b' -- U+0062
  , T.singleton <$> P.single 'c' -- U+0063
  , T.singleton <$> P.single 'd' -- U+0064
  , T.singleton <$> P.single 'e' -- U+0065
  , T.singleton <$> P.single 'f' -- U+0066
  , T.singleton <$> P.single 'g' -- U+0067
  , T.singleton <$> P.single 'h' -- U+0068
  , T.singleton <$> P.single 'i' -- U+0069
  , T.singleton <$> P.single 'j' -- U+006a
  , T.singleton <$> P.single 'k' -- U+006b
  , T.singleton <$> P.single 'l' -- U+006c
  , T.singleton <$> P.single 'm' -- U+006d
  , T.singleton <$> P.single 'n' -- U+006e
  , T.singleton <$> P.single 'o' -- U+006f
  , T.singleton <$> P.single 'p' -- U+0070
  , T.singleton <$> P.single 'q' -- U+0071
  , T.singleton <$> P.single 'r' -- U+0072
  , T.singleton <$> P.single 's' -- U+0073
  , T.singleton <$> P.single 't' -- U+0074
  , T.singleton <$> P.single 'u' -- U+0075
  , T.singleton <$> P.single 'v' -- U+0076
  , T.singleton <$> P.single 'w' -- U+0077
  , T.singleton <$> P.single 'x' -- U+0078
  , T.singleton <$> P.single 'y' -- U+0079
  , T.singleton <$> P.single 'z' -- U+007a
  -- , T.singleton <$> P.single '{' -- U+007b
  , T.singleton <$> P.single '|' -- U+007c
  -- , T.singleton <$> P.single '}' -- U+007d
  , T.singleton <$> P.single '~' -- U+007e
  , T.singleton <$> P.single 'Δ' -- U+0394
  , T.singleton <$> P.single 'Θ' -- U+0398
  , T.singleton <$> P.single 'Λ' -- U+039b
  , T.singleton <$> P.single 'Ξ' -- U+039e
  , T.singleton <$> P.single 'Π' -- U+03a0
  , T.singleton <$> P.single 'Σ' -- U+03a3
  , T.singleton <$> P.single 'Υ' -- U+03a5
  , T.singleton <$> P.single 'Φ' -- U+03a6
  , T.singleton <$> P.single 'Χ' -- U+03a7
  , T.singleton <$> P.single 'Ψ' -- U+03a8
  , T.singleton <$> P.single 'Ω' -- U+03a9
  , T.singleton <$> P.single 'α' -- U+03b1
  , T.singleton <$> P.single 'β' -- U+03b2
  , T.singleton <$> P.single 'γ' -- U+03b3
  , T.singleton <$> P.single 'δ' -- U+03b4
  , T.singleton <$> P.single 'ε' -- U+03b5
  , T.singleton <$> P.single 'ζ' -- U+03b6
  , T.singleton <$> P.single 'η' -- U+03b7
  , T.singleton <$> P.single 'θ' -- U+03b8
  , T.singleton <$> P.single 'κ' -- U+03ba
  , T.singleton <$> P.single 'λ' -- U+03bb
  , T.singleton <$> P.single 'μ' -- U+03bc
  , T.singleton <$> P.single 'ν' -- U+03bd
  , T.singleton <$> P.single 'ξ' -- U+03be
  , T.singleton <$> P.single 'ο' -- U+03bf
  , T.singleton <$> P.single 'π' -- U+03c0
  , T.singleton <$> P.single 'ρ' -- U+03c1
  , T.singleton <$> P.single 'σ' -- U+03c3
  , T.singleton <$> P.single 'τ' -- U+03c4
  , T.singleton <$> P.single 'υ' -- U+03c5
  , T.singleton <$> P.single 'φ' -- U+03c6
  , T.singleton <$> P.single 'χ' -- U+03c7
  , T.singleton <$> P.single 'ψ' -- U+03c8
  , T.singleton <$> P.single 'ω' -- U+03c9
  , T.singleton <$> P.single 'ア' -- U+30a2
  , T.singleton <$> P.single 'イ' -- U+30a4
  , T.singleton <$> P.single 'オ' -- U+30aa
  , T.singleton <$> P.single 'グ' -- U+30b0
  , T.singleton <$> P.single 'ジ' -- U+30b8
  , T.singleton <$> P.single 'チ' -- U+30c1
  , T.singleton <$> P.single 'テ' -- U+30c6
  , T.singleton <$> P.single 'ド' -- U+30c9
  , T.singleton <$> P.single 'バ' -- U+30d0
  , T.singleton <$> P.single 'ビ' -- U+30d3
  , T.singleton <$> P.single 'プ' -- U+30d7
  , T.singleton <$> P.single 'ム' -- U+30e0
  , T.singleton <$> P.single 'メ' -- U+30e1
  , T.singleton <$> P.single 'ラ' -- U+30e9
  , T.singleton <$> P.single 'リ' -- U+30ea
  , T.singleton <$> P.single 'ル' -- U+30eb
  , T.singleton <$> P.single 'レ' -- U+30ec
  , T.singleton <$> P.single 'ン' -- U+30f3
  , T.singleton <$> P.single '・' -- U+30fb
  , T.singleton <$> P.single 'ー' -- U+30fc
  , T.singleton <$> P.single '不' -- U+4e0d
  , T.singleton <$> P.single '人' -- U+4eba
  , T.singleton <$> P.single '他' -- U+4ed6
  , T.singleton <$> P.single '代' -- U+4ee3
  , T.singleton <$> P.single '作' -- U+4f5c
  , T.singleton <$> P.single '前' -- U+524d
  , T.singleton <$> P.single '副' -- U+526f
  , T.singleton <$> P.single '劇' -- U+5287
  , T.singleton <$> P.single '助' -- U+52a9
  , T.singleton <$> P.single '動' -- U+52d5
  , T.singleton <$> P.single '句' -- U+53e5
  , T.singleton <$> P.single '号' -- U+53f7
  , T.singleton <$> P.single '名' -- U+540d
  , T.singleton <$> P.single '品' -- U+54c1
  , T.singleton <$> P.single '商' -- U+5546
  , T.singleton <$> P.single '国' -- U+56fd
  , T.singleton <$> P.single '地' -- U+5730
  , T.singleton <$> P.single '定' -- U+5b9a
  , T.singleton <$> P.single '尾' -- U+5c3e
  , T.singleton <$> P.single '店' -- U+5e97
  , T.singleton <$> P.single '形' -- U+5f62
  , T.singleton <$> P.single '戯' -- U+622f
  , T.singleton <$> P.single '投' -- U+6295
  , T.singleton <$> P.single '接' -- U+63a5
  , T.singleton <$> P.single '新' -- U+65b0
  , T.singleton <$> P.single '施' -- U+65bd
  , T.singleton <$> P.single '族' -- U+65cf
  , T.singleton <$> P.single '映' -- U+6620
  , T.singleton <$> P.single '曲' -- U+66f2
  , T.singleton <$> P.single '書' -- U+66f8
  , T.singleton <$> P.single '格' -- U+683c
  , T.singleton <$> P.single '標' -- U+6a19
  , T.singleton <$> P.single '機' -- U+6a5f
  , T.singleton <$> P.single '氏' -- U+6c0f
  , T.singleton <$> P.single '民' -- U+6c11
  , T.singleton <$> P.single '源' -- U+6e90
  , T.singleton <$> P.single '演' -- U+6f14
  , T.singleton <$> P.single '漫' -- U+6f2b
  , T.singleton <$> P.single '物' -- U+7269
  , T.singleton <$> P.single '画' -- U+753b
  , T.singleton <$> P.single '略' -- U+7565
  , T.singleton <$> P.single '番' -- U+756a
  , T.singleton <$> P.single '省' -- U+7701
  , T.singleton <$> P.single '短' -- U+77ed
  , T.singleton <$> P.single '約' -- U+7d04
  , T.singleton <$> P.single '組' -- U+7d44
  , T.singleton <$> P.single '結' -- U+7d50
  , T.singleton <$> P.single '続' -- U+7d9a
  , T.singleton <$> P.single '縮' -- U+7e2e
  , T.singleton <$> P.single '織' -- U+7e54
  , T.singleton <$> P.single '聞' -- U+805e
  , T.singleton <$> P.single '自' -- U+81ea
  , T.singleton <$> P.single '船' -- U+8239
  , T.singleton <$> P.single '著' -- U+8457
  , T.singleton <$> P.single '記' -- U+8a18
  , T.singleton <$> P.single '設' -- U+8a2d
  , T.singleton <$> P.single '試' -- U+8a66
  , T.singleton <$> P.single '誌' -- U+8a8c
  , T.singleton <$> P.single '語' -- U+8a9e
  , T.singleton <$> P.single '資' -- U+8cc7
  , T.singleton <$> P.single '跡' -- U+8de1
  , T.singleton <$> P.single '連' -- U+9023
  , T.singleton <$> P.single '遺' -- U+907a
  , T.singleton <$> P.single '間' -- U+9593
  , T.singleton <$> P.single '雑' -- U+96d1
  , T.singleton <$> P.single '頭' -- U+982d
  , T.singleton <$> P.single '験' -- U+9a13
  , T.singleton <$> P.single 'ｙ' -- U+ff59
  ]
