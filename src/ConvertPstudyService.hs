module ConvertPstudyService (
  runConvertPstudyService,
) where

import Import
import qualified RIO.Text as T
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO as T
import Text.Show.Unicode (uprint)
import DictionaryParser as P

runConvertPstudyService :: (FilePath, Natural) -> RIO App ()
runConvertPstudyService (path, level) = do
  when (level > 12) $ do
    logError "invalid svl number. level should be 1 to 12. @see https://www.alc.co.jp/vocgram/article/svl/"
    exitFailure
  env <- ask

  conv <- liftIO $ ICU.open "Shift-JIS" Nothing
  runConduitRes $ sourceFile path
    .| CB.lines
    .| mapC (ICU.toUnicode conv)
    .| mapC (T.strip)
    .| mapMC (\input -> liftIO (T.putStrLn input) >> pure input)
    .| mapC P.runDictionaryParser
    .| mapM_C (\case
        Right a -> (liftIO . uprint $ a) >> (liftIO . T.putStrLn $ "")
        Left e -> liftIO . T.putStrLn $ P.tshowParseErrorBundle e
    )

  when (env.appOptions.optionsVerbose) $ do
    logInfo "ConvertPstudyService"
    logInfo $ "path = " <> displayShow path
    logInfo $ "level = " <> displayShow level
    logInfo $ displayShow env.appOptions
