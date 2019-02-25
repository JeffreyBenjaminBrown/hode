module UI.Clipboard where

import Data.Functor (void)
import GHC.IO.Handle (hGetContents)
import System.Clipboard (getClipboardString)
import System.Process (createProcess_, shell)


toClipboard :: String -> IO ()
toClipboard s = void $ createProcess_ "toClipboard"
  $ shell $ "echo " ++ show s ++ " | xsel --clipboard"

fromClipboard :: IO String
fromClipboard = maybe "" id <$> getClipboardString

-- | https://www.reddit.com/r/haskellquestions/comments/aueqp7/how_to_send_stdout_into_a_string_variable/
_fromClipboard_broken :: IO (Either String String)
_fromClipboard_broken = do
  (mbStdIn,_,_,_) <- createProcess_ "fromClipboard"
                 $ shell "xsel -o --clipboard"
  case mbStdIn of
    Nothing -> return $ Left "fromClipboard: error."
    Just h -> hGetContents h >>= return . Right
