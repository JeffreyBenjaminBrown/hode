module UI.Clipboard where

import Data.Functor (void)
import System.Process (readProcess, createProcess_, shell)


toClipboard :: String -> IO ()
toClipboard s = void $ createProcess_ "toClipboard"
  $ shell $ "echo " ++ show s ++ " | xsel --clipboard"

fromClipboard :: IO String
fromClipboard = readProcess "xsel" ["-o","--clipboard"] ""

-- import System.Clipboard (getClipboardString)
-- _fromClipboard :: IO String
-- _fromClipboard = maybe "" id <$> getClipboardString
