module Hode.Hash.Parse.Util where

import           Data.List (intercalate)
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
  { hashKeyword_name :: String
  , hashKeyword_symbols :: [HashSymbol]
  , hashKeyword_help :: String }
  deriving (Show, Eq, Ord)

rep :: Int -> HashSymbol -> String
rep n hs =
  (if _slashPrefix hs then "/" else "")
  ++ concat (replicate n $ _rawSymbol hs)

reph :: Int -> HashKeyword -> String
reph n hk = rep n $ head $ hashKeyword_symbols hk

pThisMany :: Int -> HashKeyword -> Parser ()
pThisMany n hk =
  let hss :: [HashSymbol]
      hss = hashKeyword_symbols hk
      p1 :: HashSymbol -> Parser ()
      p1 hs = string (rep n hs)
        <* notFollowedBy (string $ _rawSymbol hs)
          -- PITFALL: This looks like `nonPrefix`, but it's not.
          -- `nonPrefix` ensures that a string is followed by space or ().
          -- This just ensures it's not followed by another copy of itself.
          -- That's important for parsing, e.g., the # in "this #is tricky",
          -- or the "/i" in "/i-2".
        >> return ()
  in foldl1 (<|>) $ map (try . p1) hss

hashSymbol_withSlash :: String -> HashSymbol
hashSymbol_withSlash s =
  HashSymbol { _rawSymbol = s
             , _slashPrefix = True }

-- | Prefixes to the help blurb a caveat,
-- that there are equivalent synonyms to the keyword it describes.
synonymBlurb :: HashKeyword -> (String -> String)
synonymBlurb hk = let
  syms :: [HashSymbol] = hashKeyword_symbols hk
  ss :: String = intercalate ", " $ map (rep 1) syms
  in case length syms of
       0 -> error "A HashKeyword should define at least one symbol."
       1 -> id
       _ -> (++) $ "(The keywords {" ++ ss ++ "} are all synonyms; they do exactly the same thing, and it doesn't matter which one you use.)\n\n"
