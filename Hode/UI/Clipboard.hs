module Hode.UI.Clipboard where

import Data.Functor (void)
import System.Process (readProcess, createProcess_, shell)


toClipboard :: String -> IO ()
toClipboard s = void $ createProcess_ "toClipboard"
  $ shell $ "echo " ++ show s ++ " | xsel --clipboard"

fromClipboard :: IO String
fromClipboard = readProcess "xsel" ["-o","--clipboard"] ""

-- | If I for some reason don't want to use System.Process,
-- I've got reading working using System.Clipboard (below),
-- but not writing.
--
-- import System.Clipboard (getClipboardString)
-- _fromClipboard :: IO String
-- _fromClipboard = maybe "" id <$> getClipboardString
