module Dict (
  DictHeader (..),
  DictAttr (..),
  -- word,
  -- translated,
  -- svl,
  -- phonetics,
  -- mispronounce,
) where

import RIO

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

{-
word :: Dict a -> Text
word = loop
  where
    loop :: Dict a -> Text
    loop = \case
      Word a -> a
      Translated _ a -> loop a
      Svl _ a -> loop a
      Pron _ a -> loop a
      Mispron _ a -> loop a

translated :: Dict a -> Maybe Text
translated = loop
  where
    loop = \case
      Word _ -> Nothing
      Translated a _ -> Just a
      Svl _ a -> loop a
      Pron _ a -> loop a
      Mispron _ a -> loop a

svl :: Dict a -> Maybe Natural
svl = loop
  where
    loop = \case
      Word _ -> Nothing
      Translated _ a -> loop a
      Svl a _ -> Just a
      Pron _ a -> loop a
      Mispron _ a -> loop a

phonetics :: Dict a -> Maybe Text
phonetics = loop
  where
    loop = \case
      Word _ -> Nothing
      Translated _ a -> loop a
      Svl _ a -> loop a
      Pron a _ -> Just a
      Mispron _ a -> loop a

mispronounce :: Dict a -> Maybe Bool
mispronounce = loop
  where
    loop = \case
      Word _ -> Nothing
      Translated _ a -> loop a
      Svl _ a -> loop a
      Pron _ a -> loop a
      Mispron a _ -> Just a
-}
