-- | If I for some reason one did not want to use System.Process,
-- here's how to read using System.Clipboard, which is available for Windows and Unix.
-- I haven't figured out how to get writing working with it, though.
--
-- import System.Clipboard (getClipboardString)
-- _fromClipboard :: IO String
-- _fromClipboard = maybe "" id <$> getClipboardString

module Hode.UI.Clipboard where

import Data.Functor (void)
import System.Process (readProcess, createProcess_, shell)


toClipboard :: String -> IO ()
toClipboard s = void $ createProcess_ "toClipboard"
  $ shell $ "echo " ++ show s ++ " | xsel --clipboard"

fromClipboard :: IO String
fromClipboard = readProcess "xsel" ["-o","--clipboard"] ""
