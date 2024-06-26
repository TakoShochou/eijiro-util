module Service.ConvertPstudyService (
  runConvertPstudyService,
) where

import Import
import Data.List (sortOn)
import Conduit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON

import qualified Model.Dict as D
import qualified Parser.DictionaryParser as P

-- TODO use criterion package and runMode function to measure this service
-- TODO make format option type safe

runConvertPstudyService :: (Text, FilePath, FilePath, Natural, String, String, String) -> RIO App ()
runConvertPstudyService (appInfo, readPath, writePath, level, srcEncoding, destEncoding, outputFormat) = do

  env <- ask
  when (env.appOptions.optionsVerbose) $ do
    liftIO . printE $ "processing: ConvertPstudyService begings"

  when (level > 12) $ do
    logError "invalid svl number. level should be 1 to 12. @see https://www.alc.co.jp/vocgram/article/svl/"
    exitFailure

  srcConv <- liftIO $ ICU.open srcEncoding Nothing
  destConv <- liftIO $ ICU.open destEncoding Nothing

  unless (outputFormat `elem` ["csv", "json"]) $ do
    logError "invalid output format. you can specify csv or json."
    exitFailure

  result :: [D.DictEntry] <- runConduitRes $ sourceFile readPath
    .| CB.lines
    .| mapC (ICU.toUnicode srcConv)
    .| mapC T.strip
    .| mapC P.runDictionaryParser
    .| filterMC filterAndReportLeft
    .| mapC unliftParserResult
    .| sinkList
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: all the dictionary data encodings have been converted from Shift JIS to UTF 8, and parsed"

  let svl :: [D.DictEntry] = if level == 0
      then sortBySvl $ filter (filterSvl level) result
      else filter (filterSvl level) result
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: svl entries have been extracted from the parsed data."

  let group :: [(D.DictEntry, [D.DictEntry])] = groupifyDictEntries svl result
  when (env.appOptions.optionsVerbose) $
    liftIO $ printE "processing: dictionary data listed in SVL have been grouped"

  -- TODO too slow to write
  -- @see https://stackoverflow.com/questions/55120077/how-to-write-big-file-efficiently-in-haskell
  bracket
    (if writePath == "" then pure stdout else openFile writePath WriteMode)
    (\h -> if writePath == "" then pure () else hClose h)
    $ \h -> do -- inside bracket
      if outputFormat == "json"
        then do
          liftIO . T.hPutStrLn h $ "{\n\"questions\": ["
          runConduitRes $ yieldMany group
            .| mapC dictEntryToJson
            .| intersperseC ",\n"
            .| mapC (if ICU.getName destConv == "UTF-8" then T.encodeUtf8 else ICU.fromUnicode destConv)
            .| sinkHandle h
          liftIO . T.hPutStrLn h $ "]}"
        else do
          liftIO . T.hPutStrLn h $ "psscsvfile,100,,"
          liftIO . T.hPutStrLn h $ "SVL"
            <> (if level == 0 then "" else tshow level <> "000")
            <> ",https://eijiro.jp/,"
            <> appInfo <> ","
          liftIO . T.hPutStrLn h $ ",,,"
          liftIO . T.hPutStrLn h $ "a1,h1,h2,q1"
          runConduitRes $ yieldMany group
            .| mapC dictEntryToCsv
            .| mapC (<> "\n")
            .| mapC (if ICU.getName destConv == "UTF-8" then T.encodeUtf8 else ICU.fromUnicode destConv)
            .| sinkHandle h

  when (env.appOptions.optionsVerbose) $ do
    liftIO . printE $ "processing: ConvertPstudyService ends"
    liftIO . printE $ "src path = " <> T.pack readPath
    liftIO . printE $ "dest path = " <> T.pack writePath
    liftIO . printE $ "level = " <> tshow level
    liftIO . printE $ "src converter = " <> (T.pack . ICU.getName) srcConv
    liftIO . printE $ "dest converter = " <> (T.pack . ICU.getName) destConv
    liftIO . printE $ "output format = " <> tshow outputFormat
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
        fromMaybe 0 (D.svl $ snd x)
      )

    groupifyDictEntries :: [D.DictEntry] -> [D.DictEntry] -> [(D.DictEntry, [D.DictEntry])]
    groupifyDictEntries svls xs =
      map f1 svls
      where
        f1 :: D.DictEntry -> (D.DictEntry, [D.DictEntry])
        f1 svl = (svl, flip filter xs $ \x -> (D.word . fst $ svl) == (D.word . fst $ x))

    --
    -- output
    --

    showSvlText ::  D.DictAttr a -> Text
    showSvlText attr = maybe "" tshow (D.svl attr)

    showSvlNatural :: D.DictAttr a -> Natural
    showSvlNatural attr = maybe 0 id (D.svl attr)

    -- a1(mispron word [pron]), a2, q1([label] translated...)
    dictEntryToCsv :: (D.DictEntry, [D.DictEntry]) -> Text
    dictEntryToCsv ((svlHeader, svlBody), dict) =
      (T.replace "," "、" . D.word) svlHeader
      <> ","
      <> showPron svlBody
      <> ","
      <> "SVL" <> showSvlText svlBody
      <> ","
      <> (T.replace "," "、" . showTranslated) dict
      where
        showPron :: D.DictAttr a -> Text
        showPron attr =
          case (D.mispron attr, D.pron attr) of
            ("", "") -> ""
            ("", pron) -> pron
            (mispron, "") -> mispron <> "【発音注意】"
            (pron, mispron) -> max pron mispron <> "【発音注意】"
        showTranslated :: [D.DictEntry] -> Text
        showTranslated entries = T.intercalate " " . filter (/= "") . flip map entries $ \x -> do
          let l :: Text = (T.strip . D.label . fst) x
          let t :: Text = (T.strip . D.translated . snd) x
          case (l, t) of
            ("", "") -> ""
            ("", _) -> "" <> t
            (_, "") -> "[" <> l <> "]"
            _ -> "[" <> l <> "]" <> t

    -- {
    --   "questions": [
    --     {
    --       "header": "aaaa",
    --       "level": 1,
    --       "body": [
    --         {
    --           "translated": "あああ",
    --           "pron": "a:",
    --           "mispron": true
    --         }
    --       ] // body
    --     }
    --   ] // questions
    -- }
    --
    --
    --
    dictEntryToJson :: (D.DictEntry, [D.DictEntry]) -> Text
    dictEntryToJson ((svlHeader, svlBody), dict) =
        (T.decodeUtf8 . BSL.toStrict . JSON.encodePretty . JSON.object) entry
      where
        entry = [ "header" JSON..= (((T.replace "," "、" . D.word) svlHeader) :: Text)
          , "level" JSON..= (showSvlNatural svlBody :: Natural)
          , "body" JSON..= body
          ]
        body = filter (/= " ") . flip map dict $ \x -> do
          let l :: Text = (T.strip . D.label . fst) x
          let t :: Text = (T.strip . D.translated . snd) x
          let (p, m) :: (Text, Bool) =
                case (D.pron svlBody, D.mispron svlBody) of
                  ("", "") -> ("", False)
                  (pron, "") -> (pron, False)
                  ("", mispron) -> (mispron, True)
                  (pron, mispron) -> (max pron mispron, True)
          if t == ""
            then JSON.Null
              -- ^ TODO remove an empty entry from JSON output
              -- @see https://hackage.haskell.org/package/aeson-2.2.1.0/docs/Data-Aeson.html#v:omitNothingFields
            else
              JSON.object
                [ "translated" JSON..= t
                , "label" JSON..= l
                , "pron" JSON..= p
                , "mispron" JSON..= m
                ]
