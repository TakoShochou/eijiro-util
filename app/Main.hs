{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Data.Version (showVersion)
import qualified RIO.Text as T
import AnalyseService
import ConvertUtf8Service
import ConvertPstudyService
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_eijiro_util
import qualified PackageInfo_eijiro_util

-- TODO use criterion package and runMode function to measure this prog

main :: IO ()
main = do
  (options, runCmd) <- simpleOptions
      $(simpleVersion Paths_eijiro_util.version)
      PackageInfo_eijiro_util.name
      PackageInfo_eijiro_util.synopsis
      (Options <$> switch pVerbose)
      (analyseCommand >> convertUtf8Command >> convertPstudyCommand)
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
    in runRIO app runCmd
  where
    pVerbose = long "verbose" <> short 'v' <> help "Verbose output"
    analyseCommand = addCommand "analyse"
      "List all the characters and their code points in the header words of the Eijiro dictionary data"
      runAnalyseService
      $ (,) <$> pReadPath <*> pTarget
    convertUtf8Command = addCommand "utf8"
      "Convert Eijiro dictionary encoding from Shift JIS to UTF8"
      runConvertUtf8Service
      $ (,,) <$> pReadPath <*> pTakeFrom <*> pTakeTo
    convertPstudyCommand = addCommand "pstudy"
      "Convert Eijiro dictionary data to Pstduy data"
      runConvertPstudyService
      $ (,,,,,) <$> appInfo <*> pReadPath <*> pWritePath <*> pConvertLevel <*> pSrcEncoding <*> pDescEncoding
    appInfo :: Parser Text
    appInfo =
      pure
        $  T.pack PackageInfo_eijiro_util.name
        <> " "
        <> (T.pack . showVersion) PackageInfo_eijiro_util.version
        <> " "
        <> T.pack PackageInfo_eijiro_util.copyright
    pReadPath = strArgument (metavar "FILE_PATH" <> help "Eijiro data file")
    pWritePath = strOption
      $ short 'f'
      <> long "file"
      <> value ""
      <> metavar "FILE_PATH"
      <> help "specify pstudy file path to be saved"
    pSrcEncoding = strOption
      $  long "src_encoding"
      <> value "Shift-JIS"
      <> metavar "UTF-8|Shift-JIS"
      <> help "specify encoding of input file"
      <> showDefault
    pDescEncoding = strOption
      $ long "dest_encoding"
      <> value "Shift-JIS"
      <> metavar "UTF-8|Shift-JIS"
      <> help "specify encoding of output file"
      <> showDefault
    pTarget = strOption
      $  short 't'
      <> long "target"
      <> value "header"
      <> metavar "header|label|attr"
      <> help "specify part of the line to be analysed: header word, header speech labels, or body attributes"
    pConvertLevel = option auto
      $  short 'l'
      <> long "level"
      <> value 0
      <> metavar "NUMBER"
      <> help "SVL number: from 1 to 12"
    pTakeFrom = option auto
      $  long "offset"
      <> value 0
      <> metavar "NUMBER"
      <> help "specify begin line to be taken"
    pTakeTo = option auto
      $  short 'n'
      <> long "take"
      <> value 9_999_999
      <> metavar "NUMBER"
      <> help "specify end line to be taken"
