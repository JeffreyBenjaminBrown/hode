{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Temp where

--import qualified Data.Map              as M
--import qualified Data.List             as L
--import qualified Data.List.PointedList as P
--import qualified Data.Set              as S
--import           Control.Lens
--import           Data.Set (Set)
--import qualified Data.Set              as S
--import Hode.Hode

import           Control.Monad (void)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.List (intersperse)
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import Hode.Hash.EitherExpr
import Hode.Hash.Hash
import Hode.Hash.Types
import Hode.Hash.Util
import Hode.Qseq.Types (Var(..))
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.Util.Alternation
import Hode.Util.Parse

import Hode.Hash.Parse


-- | like `phrase`, but includes every character that's not special
-- Hash syntax.
hashPhrase :: Parser String
hashPhrase =
  quoted
  <|> concat . intersperse " " <$> some hashIdentifier
 where

  quoted :: Parser String
  -- A quoted string can have any character in it.
  -- Some of those might need escaping (e.g. \" and \\).
  -- This comes nearly verbatim from
  -- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
  quoted = do void $ char '"'
              strings <- many quotedCharacter
              void $ char '"'
              return $ concat strings

  quotedCharacter :: Parser String
  quotedCharacter = fmap return nonEscape <|> escape

  escape :: Parser String
  escape = do
      void $ char '\\'
      c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return [c]

  nonEscape :: Parser Char
  nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

