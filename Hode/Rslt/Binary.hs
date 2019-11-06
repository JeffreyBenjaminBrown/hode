module Hode.Rslt.Binary where


-- | Used in sorting and transitive searches, for binary `Tplt`s.

data SearchDir = SearchLeftward | SearchRightward
  deriving (Show, Eq, Ord)

data BinMember = LeftMember | RightMember
  deriving (Show, Eq, Ord)

data BinOrientation = RightFirst | LeftFirst
