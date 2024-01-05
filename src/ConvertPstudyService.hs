module ConvertPstudyService (
  runConvertPstudyService,
) where

import Import
import qualified RIO.Text as T
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO as T
import Text.Show.Unicode (uprint, ushow)
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
  result :: [P.DictEntry] <- runConduitRes $ sourceFile readPath
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
  let svl = filter (filterSvl level) result
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: svl entries are extracted from the parsed data."

  -- TODO use async
  let group :: [(P.DictEntry, [P.DictEntry])] = groupifyDictEntries svl result
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: dictionary data listed in SVL are grouped"

  --
  -- TODO too slow for writing @see https://stackoverflow.com/questions/55120077/how-to-write-big-file-efficiently-in-haskell
  --
  runConduitRes $ yieldMany group
    .| mapC (U.fromString . ushow)
    .| mapC (<> "\n")
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

    unliftParserResult :: P.ParserResult -> P.DictEntry
    unliftParserResult (Left _) = undefined
    unliftParserResult (Right (h, a)) = (h, a)

    filterSvl :: Natural -> P.DictEntry -> Bool
    filterSvl targetLevel (_, attr) = do
      let l = D.svl attr
      case l of
        Nothing -> False
        Just l' -> targetLevel == 0 || l' == targetLevel

    groupifyDictEntries :: [P.DictEntry] -> [P.DictEntry] -> [(P.DictEntry, [P.DictEntry])]
    groupifyDictEntries svls xs =
      map f1 svls
      where
        f1 :: P.DictEntry -> (P.DictEntry, [P.DictEntry])
        f1 svl = (svl, flip filter xs $ \x -> (D.word . fst $ svl) == (D.word . fst $ x))
