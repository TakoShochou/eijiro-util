{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Service.ConvertPstudyService (
  runConvertPstudyService,
  OutputFormat(..),
  ParserStrategy(..),
) where

import qualified Model.Dict as D

import Import
import qualified System.Console.ANSI as ANSI
import Data.List ( sortOn )
import qualified Data.Map as Map
import Conduit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON

import qualified Parser.MegaParsecDictionaryParser as MPDP

data OutputFormat = CSV | JSON deriving (Eq, Show)
data ParserStrategy = MegaParsec | TypedPeg deriving (Eq, Show)

-- TODO use criterion package and runMode function to measure this service
runConvertPstudyService :: (Text, FilePath, FilePath, Natural, String, String, OutputFormat, ParserStrategy) -> RIO App ()
runConvertPstudyService (appInfo, readPath, writePath, level, srcEncoding, destEncoding, outputFormat, parserStrategy) = do

  env <- ask

  let isVerbose :: Bool = env.appOptions.optionsVerbose

  when isVerbose $ do
    liftIO . printE $ "processing: ConvertPstudyService begins"

  when (level > 12) $ do
    logError "invalid svl number. level should be 1 to 12. @see https://www.alc.co.jp/vocgram/article/svl/"
    exitFailure

  srcConv <- liftIO $ ICU.open srcEncoding Nothing
  destConv <- liftIO $ ICU.open destEncoding Nothing

  wholeFlattenDictEntries :: [D.DictEntry] <- runConduitRes $ sourceFile readPath
    .| CB.lines
    .| numberLines
    .| mapC (second . ICU.toUnicode $ srcConv)
    .| mapC (second T.strip)
    .| runParser
    .| ( if isVerbose
        then iterMC (liftIO . printProgress)
        else awaitForever yield
       )
    .| mapC snd
    .| sinkList

  when isVerbose $ do
    liftIO $ printE ""
    liftIO $ printE "processing: all the dictionary data have been converted from Shift JIS to UTF 8, and parsed"

  let svl :: [D.DictEntry] = if level == 0
      then sortBySvl $ filter (filterSvl level) wholeFlattenDictEntries
      else filter (filterSvl level) wholeFlattenDictEntries
  when isVerbose $
    liftIO $ printE "processing: SVL entries have been extracted from the parsed data (evaluation may be deferred due to lazy evaluation)."

  let group :: [(D.DictEntry, [D.DictEntry])] = groupifyDictEntries svl wholeFlattenDictEntries
  when isVerbose $
    liftIO $ printE "processing: dictionary data listed in SVL have been grouped (evaluation may be deferred due to lazy evaluation)."

  bracket
    (if writePath == "" then pure stdout else openFile writePath WriteMode)
    (\h -> if writePath == "" then pure () else hClose h)
    $ \h -> do -- inside bracket
      if outputFormat == JSON
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

  when isVerbose $ do
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

    printProgress :: (Natural, D.DictEntry) -> IO ()
    printProgress (lineNum, (head, attr)) = do
      ANSI.hSetCursorColumn stderr 0
      ANSI.hClearLine stderr
      T.hPutStr stderr template
      hFlush stderr
      where
        svl = case D.svl attr of
          Just n  -> " (svl" <> tshow n <> ")"
          Nothing -> ""
        template =
          "processing: parsed [" <> tshow lineNum <> "] " <> D.word head <> svl

    numberLines ::ConduitT a (Natural, a) _ ()
    numberLines = loop 1
      where
        loop n = await >>= \case
          Nothing -> pure ()
          Just x -> do
            yield (n, x)
            loop (n + 1)

    runParser :: ConduitT (Natural, Text) (Natural, D.DictEntry) _ ()
    runParser =
      case parserStrategy of
        MegaParsec -> parseDictionaryByMegaParsec
        TypedPeg -> parseDictionaryByTypedPeg
      where
        parseDictionaryByMegaParsec :: ConduitT (Natural, Text) (Natural, D.DictEntry) _ ()
        parseDictionaryByMegaParsec =
          mapC (second MPDP.runDictionaryParser)
          .| filterMC (\(_, result) -> filterAndReportLeft result)
          .| mapC (second unliftParserResult)
          where
            filterAndReportLeft :: MPDP.ParserResult -> _ Bool
            filterAndReportLeft =
              \case
                Right _ -> pure True
                Left e -> do
                  liftIO . printE $ ""
                  liftIO . printE . MPDP.tshowParseErrorBundle $ e
                  pure False

            unliftParserResult :: MPDP.ParserResult -> D.DictEntry
            unliftParserResult (Left _) = undefined
            unliftParserResult (Right (h, a)) = (h, a)

        parseDictionaryByTypedPeg :: ConduitT (Natural, Text) (Natural, D.DictEntry) _ ()
        parseDictionaryByTypedPeg =
          mapC $ error "TODO: implement parseDictionaryByTypedPeg"

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

    -- takes `svls`, a list of parsed SVL entries, and `xs`, a flat list of all parsed dic dictionary
    -- entries.
    -- returns a list of tuples, each containing as SVL entry as the key and a list of all dictionary
    -- entries with the same headword as the value.
    groupifyDictEntries :: [D.DictEntry] -> [D.DictEntry] -> [(D.DictEntry, [D.DictEntry])]
    groupifyDictEntries svls xs =
      map f1 svls
      where
        f1 :: D.DictEntry -> (D.DictEntry, [D.DictEntry])
        f1 svl = (svl, maybe [] reverse $ Map.lookup (D.word (fst svl)) dict)
        -- xsから見出し語をキーとした辞書を生成し、後の処理でのルックアップ速度向上を図ります。
        dict :: Map Text [D.DictEntry]
        dict = Map.fromListWith (<>) [ (D.word (fst x), [x]) | x <- xs]

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

    dictEntryToJson :: (D.DictEntry, [D.DictEntry]) -> Text
    dictEntryToJson ((svlHeader, svlBody), dict) =
        (T.decodeUtf8 . BSL.toStrict . JSON.encodePretty . JSON.object) entry
      where
        entry = [ "header" JSON..= ((T.replace "," "、" . D.word) svlHeader :: Text)
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
