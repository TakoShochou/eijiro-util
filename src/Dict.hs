module Dict (
  DictEntry,
  DictHeader (..),
  DictAttr (..),
  word,
  label,
  translated,
  svl,
  -- phonetics,
  -- mispronounce,
) where

import RIO

-- @see https://www.eijiro.jp/spec.htm

type DictEntry = (DictHeader Text, DictAttr Text)

data DictHeader a where
  Word :: Text -> DictHeader a -- unit element
  Index :: Natural -> DictHeader a ->  DictHeader a
  Label :: Text -> DictHeader a -> DictHeader a -- ラベル１・ラベル２
  LabelIndex :: Natural -> DictHeader a -> DictHeader a
  LabelIndexIndex :: Natural -> DictHeader a -> DictHeader a

data DictAttr a where
  Ignore :: (Text, Text) -> DictAttr a -> DictAttr a -- ignored header
  Translated :: Text -> DictAttr a -- unit element
  Svl :: Natural -> DictAttr a -> DictAttr a
  Pron :: Text -> DictAttr a -> DictAttr a
  Mispron :: Text -> DictAttr a -> DictAttr a

deriving instance Show a => Show (DictHeader a)
deriving instance Show a => Show (DictAttr a)

-- DictHeader accessors

word :: DictHeader a -> Text
word = loop
  where
    loop :: DictHeader a -> Text
    loop = \case
      Word a -> a
      Index _ a -> loop a
      Label _ a -> loop a
      LabelIndex _ a -> loop a
      LabelIndexIndex _ a -> loop a

label :: DictHeader a -> Text
label = loop
  where
    loop :: DictHeader a -> Text
    loop = \case
      Word _ -> ""
      Index _ a -> loop a
      Label a _ -> a
      LabelIndex _ a -> loop a
      LabelIndexIndex _ a -> loop a

-- DictAttr accessors

translated :: DictAttr a -> Text
translated = loop
  where
    loop = \case
      Ignore _ a -> loop a
      Translated a -> a
      Svl _ a -> loop a
      Pron _ a -> loop a
      Mispron _ a -> loop a

svl :: DictAttr a -> Maybe Natural
svl = loop
  where
    loop :: DictAttr a -> Maybe Natural
    loop = \case
      Ignore _ a -> loop a
      Translated _ -> Nothing
      Svl hit _ -> Just hit
      Pron _ a -> loop a
      Mispron _ a -> loop a
