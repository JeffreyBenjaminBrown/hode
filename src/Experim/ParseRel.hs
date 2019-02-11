-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ScopedTypeVariables #-}

module Experim where

import           Control.Monad (void)
import           Control.Monad.Combinators.Expr
import           Data.List (intersperse)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Util.Parse


type Level = Int
type Joint = String

data PRel -- ^ a parser for `Rel`s
  = Absent -- ^ The leftmost and rightmost members of an `Open` or `Closed`
    -- might be absent. Interior ones should not be.
  | Leaf String
  | Closed     [(PRel,Joint)] PRel -- ^ The list should not be empty.
    -- If the list has length 1, there are no interior members.
  | Open Level [(PRel,Joint)] PRel -- ^ Like `Closed`, but more things
    -- might be inserted into it.
  deriving (Eq, Show)

higher :: PRel -> PRel -> Bool
higher (Leaf _    ) (Leaf _    ) = True
higher (Leaf _    ) (Closed _ _) = False
higher (Leaf _    ) (Open _ _ _) = False
higher (Closed _ _) (Closed _ _) = True
higher (Closed _ _) (Open _ _ _) = False
higher (Open l _ _) (Open m _ _) = l <= m
higher a b = not $ higher b a

--open :: Level -> Joint -> PRel -> PRel -> PRel
--open l j a b = Open l [(
