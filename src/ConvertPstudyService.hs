module ConvertPstudyService (
  runConvertPstudyService,
) where

import Import
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO as T
import Text.Show.Unicode (ushow)
import qualified Data.ByteString.UTF8 as U

import qualified Dict as D
import qualified DictionaryParser as P

runConvertPstudyService :: (FilePath, FilePath, Natural) -> RIO App ()
runConvertPstudyService (readPath, writePath, level) = do

  env <- ask
  when (env.appOptions.optionsVerbose) $ do
    liftIO . printE $ "processing: ConvertPstudyService begings"

  when (level > 12) $ do
    logError "invalid svl number. level should be 1 to 12. @see https://www.alc.co.jp/vocgram/article/svl/"
    exitFailure

  conv <- liftIO $ ICU.open "Shift-JIS" Nothing
  result :: [D.DictEntry] <- runConduitRes $ sourceFile readPath
    .| CB.lines
    .| mapC (ICU.toUnicode conv)
    .| mapC (T.strip)
    .| mapC P.runDictionaryParser
    .| filterMC filterAndReportLeft
    .| mapC unliftParserResult
    .| sinkList
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: all the dictionary data are converted from Shift JIS encoding to UTF 8, and parsed"

  -- TODO use async
  let svl :: [D.DictEntry] = if level == 0
      then sortBySvl $ filter (filterSvl level) result
      else filter (filterSvl level) result
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: svl entries are extracted from the parsed data."

  -- TODO use async
  let group :: [(D.DictEntry, [D.DictEntry])] = groupifyDictEntries svl result
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: dictionary data listed in SVL are grouped"

  let write = if writePath == "" then T.putStrLn else T.writeFile writePath
  liftIO . write $ "psscsvfile,100,"
  liftIO . write $ "SVL" <> if level /= 0 then tshow level <> "000" else "" <> ",https://eijiro.jp/,tako.shochou@gmail.com,"
  liftIO . write $ "a1,a2,q1"

  --
  -- TODO too slow for writing @see https://stackoverflow.com/questions/55120077/how-to-write-big-file-efficiently-in-haskell
  --
  runConduitRes $ yieldMany group
    -- .| mapC (U.fromString . ushow)
    -- .| mapC ushow
    .| mapC dictEntryToText
    .| mapC (<> "\n")
    .| mapC TE.encodeUtf8
    .| if writePath == "" then stdoutC else sinkFile writePath

  when (env.appOptions.optionsVerbose) $ do
    liftIO . printE $ "processing: ConvertPstudyService ends"
    liftIO . printE $ "src path = " <> T.pack readPath
    liftIO . printE $ "dest path = " <> T.pack writePath
    liftIO . printE $ "level = " <> tshow level
    liftIO . printE $ tshow env.appOptions

  where
    printE :: Text -> IO ()
    printE = T.hPutStrLn stderr

    filterAndReportLeft :: P.ParserResult -> ResourceT (RIO App) Bool
    filterAndReportLeft =
      \case
        Right _ -> pure True
        Left e -> do
          liftIO . printE . P.tshowParseErrorBundle $ e
          pure False

    unliftParserResult :: P.ParserResult -> D.DictEntry
    unliftParserResult (Left _) = undefined
    unliftParserResult (Right (h, a)) = (h, a)

    filterSvl :: Natural -> D.DictEntry -> Bool
    filterSvl targetLevel (_, attr) = do
      let l = D.svl attr
      case l of
        Nothing -> False
        Just l' -> targetLevel == 0 || l' == targetLevel

    sortBySvl :: [D.DictEntry] -> [D.DictEntry]
    sortBySvl =
      sortOn (\x ->
        case D.svl $ snd x of
          Just x -> x
          Nothing -> 0
      )

    groupifyDictEntries :: [D.DictEntry] -> [D.DictEntry] -> [(D.DictEntry, [D.DictEntry])]
    groupifyDictEntries svls xs =
      map f1 svls
      where
        f1 :: D.DictEntry -> (D.DictEntry, [D.DictEntry])
        f1 svl = (svl, flip filter xs $ \x -> (D.word . fst $ svl) == (D.word . fst $ x))

    -- a1(word), a2(svl), q1(label-translated...)
    dictEntryToText :: (D.DictEntry, [D.DictEntry]) -> Text
    dictEntryToText ((svlHeader, svlBody), dict) =
      D.word svlHeader <> "," <> showSvl svlBody <> "," <> showTranslated dict
      where
        showSvl :: D.DictAttr a -> Text
        showSvl attr =
          case D.svl attr of
            Just x -> tshow x
            Nothing -> ""
        -- TODO use alternative to filter entries thier translated are empty
        showTranslated :: [D.DictEntry] -> Text
        showTranslated entries = T.concat . map (\x -> showLabel x <> showTranslated' x) $ entries
        showTranslated' :: D.DictEntry -> Text
        showTranslated' = D.translated . snd
        showLabel :: D.DictEntry -> Text
        showLabel =
          T.cons '[' . flip T.snoc ']' . (\x -> case x of "" -> "-"; otherwise -> x) . D.label . fst
