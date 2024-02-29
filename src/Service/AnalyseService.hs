module Service.AnalyseService (
  runAnalyseService,
  breakEvery,
) where

import Import
import Conduit
import qualified Data.Conduit.Binary as CB
import Data.List (nub, sort)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.ICU.Convert as ICU
import Text.Regex.TDFA (getAllTextMatches, (=~))

-- targetの値がheaderならば、見出し語に含まれる文字とそのコードポイントを分析します。
-- 英辞郎のデータは「■みだし語 : 〜」というフォーマットになっており、
-- このうち"■"と" : "に挟まれた部分の文字を抽出して、コードポイントを表示します。
-- targetの値がlabelならば、ヘッダーに含まれる{}で囲まれたラベルを分析します。
-- targetの値がattrならば、【】でかこまれた属性名を分析します。

runAnalyseService :: (FilePath, Text) -> RIO App ()
runAnalyseService (path, target) = do

  when (target /= "header" && target /= "label" && target /= "attr") $ do -- TODO make things type safe if needed
    logError $ "invalid argument is given to target. expect: header, label or attr. given: " <> display target
    exitFailure

  conv <- liftIO $ ICU.open "Shift-JIS" Nothing

  when (target == "header") $ do
    result :: [[Char]] <- runConduitRes $ sourceFile path
      .| CB.lines
      .| mapC (ICU.toUnicode conv)
      .| mapC goHeader
      .| sinkList
    let result' :: [Char] = sort . nub $ fold result
    mapM_ (\a -> liftIO $ printf ", P.single '%c' -- U+%04x\n" a a) result'

  when (target == "label") $ do
    result <- runConduitRes $ sourceFile path
      .| CB.lines
      .| mapC (ICU.toUnicode conv)
      .| mapC goLabel
      .| sinkList
    let result' = sort . nub . fold $ result
    forM_ result' $ liftIO . T.putStrLn

  when (target == "attr") $ do
    result <- runConduitRes $ sourceFile path
      .| CB.lines
      .| mapC (ICU.toUnicode conv)
      .| mapC goBody
      .| sinkList
    let result' = sort . nub . fold $ result
    forM_ result' $ liftIO . T.putStrLn

  liftIO $ T.putStrLn "END SERVICE"

  where
    goHeader :: Text -> [Char]
    goHeader input = do
      let input' :: Text = T.dropWhile (=='■') input
      let header :: Text = fst $ T.breakOn " : " input'
      breakEvery header

    goLabel :: Text -> [Text]
    goLabel input = do
      let header :: Text = fst $ T.breakOn " : " input
      getAllTextMatches (header =~ ("{.+}" :: Text)) :: [Text]

    goBody :: Text -> [Text]
    goBody input = do
      let body :: Text = T.drop 3 . snd . T.breakOn " : " $ input
      getAllTextMatches (body =~ ("(【[^】]+】)+" :: Text)) :: [Text]

-- "abc" -> ['a', 'b', 'c']
-- @see https://wiki.haskell.org/Foldr_Foldl_Foldl'
breakEvery :: Text -> [Char]
breakEvery = T.foldr fn []
  where
    fn :: Char -> [Char] -> [Char]
    fn ch carry = [ch] <> carry
