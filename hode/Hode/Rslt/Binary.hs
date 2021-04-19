module Hode.Rslt.Binary where


-- | * Used in sorting and transitive searches, for binary `Tplt`s.

-- | The direction in which to run a search.
-- "Left" and "right" are defined by the binary `Tplt`:
-- the first member is on the left, the second member on the right.
data SearchDir = SearchLeftward | SearchRightward
  deriving (Show, Eq, Ord)

data BinMember = LeftMember | RightMember
  deriving (Show, Eq, Ord)

-- | Describes the orientation of a partial order (see Hode.Rslt.Sort).
-- For instance, if a sort on the relation `_ #needs _`
-- is oriented `LeftEarlier`,
-- then the neediest things will be at the top of the screen,
-- and the most-needed at the bottom.
data BinOrientation = LeftEarlier | RightEarlier
  deriving (Show, Eq, Ord)
