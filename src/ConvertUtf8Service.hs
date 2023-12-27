module ConvertUtf8Service (
  runConvertUtf8Service,
) where

import Import
import Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO as T

runConvertUtf8Service :: (FilePath, Int, Int) -> RIO App ()
runConvertUtf8Service (path, offset, take) = do
  conv <- liftIO $ ICU.open "Shift-JIS" Nothing
  result <- runConduitRes $ sourceFile path
    .| CB.lines
    .| takeC take
    .| mapC (ICU.toUnicode conv)
    .| (dropC offset >> sinkList)
  forM_ result $ liftIO . T.putStrLn
  liftIO . T.putStrLn $ "END SERVICE"
  liftIO . T.putStrLn $ "path=" <> tshow path <> " offset=" <> tshow offset <> " take=" <> tshow take
