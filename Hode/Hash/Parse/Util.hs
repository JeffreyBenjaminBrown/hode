module Hode.Hash.Parse.Util where

import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char

import Hode.Util.Parse


data HashSymbol = HashSymbol
  { _rawSymbol :: String
  , _slashPrefix :: Bool }
  deriving (Show, Eq, Ord)

show' :: HashSymbol -> String
show' hs =
  (if _slashPrefix hs then "/" else "")
  ++ _rawSymbol hs

data HashKeyword = HashKeyword
  { _title :: String
  , _symbol :: [HashSymbol]
  , _help :: String }
  deriving (Show, Eq, Ord)

rep :: Int -> HashSymbol -> String
rep n hs =
  (if _slashPrefix hs then "/" else "")
  ++ concat (replicate n $ _rawSymbol hs)

reph :: Int -> HashKeyword -> String
reph n hk = rep n $ head $ _symbol hk

pThisMany :: Int -> HashKeyword -> Parser ()
pThisMany n hk =
  let hss :: [HashSymbol]
      hss = _symbol hk
      p1 :: HashSymbol -> Parser ()
      p1 hs = string (rep n hs)
              <* notFollowedBy (string $ _rawSymbol hs)
              >> return ()
  in foldl1 (<|>) $ map p1 hss

hashSymbol_withSlash :: String -> HashSymbol
hashSymbol_withSlash s =
  HashSymbol { _rawSymbol = s
             , _slashPrefix = True }
