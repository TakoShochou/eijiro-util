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

import qualified Dict as D
import qualified DictionaryParser as P

runConvertPstudyService :: (FilePath, Natural) -> RIO App ()
runConvertPstudyService (path, level) = do

  when (level > 12) $ do
    logError "invalid svl number. level should be 1 to 12. @see https://www.alc.co.jp/vocgram/article/svl/"
    exitFailure
  env <- ask

  conv <- liftIO $ ICU.open "Shift-JIS" Nothing
  result :: [P.DictEntry] <- runConduitRes $ sourceFile path
    .| CB.lines
    .| mapC (ICU.toUnicode conv)
    .| mapC (T.strip)
    .| mapC P.runDictionaryParser
    .| filterMC filterAndReportLeft
    .| mapC unliftParserResult
    .| sinkList

  let svl = filter (filterSvl level) result
  forM_ svl $ liftIO . uprint

  when (env.appOptions.optionsVerbose) $ do
    logInfo "ConvertPstudyService"
    logInfo $ "path = " <> displayShow path
    logInfo $ "level = " <> displayShow level
    logInfo $ displayShow env.appOptions

  where
    filterAndReportLeft :: P.ParserResult -> ResourceT (RIO App) Bool
    filterAndReportLeft =
      \case
        Right _ -> pure True
        Left e -> do
          liftIO . T.hPutStrLn stderr . T.pack . ushow $ e
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
