module Hode.Rslt.Sort where

type BinTpltOrder = Map Int (BinOrientation, TpltAddr)

type TopSets = [(Int,[Addr])]
-- ^ A `TopSets` is only meaningful in the context of a `BinTpltOrder`.
-- The first member of each pair is a number of Tplts in the BinTpltOrder.
-- Initially (when starting a search)
-- the only member, (0,_), represents the entire graph.
-- Whenever a new pair is pushed onto a `TopSets`,
-- its addresses will be taken (not copied) from the previous head.
-- It will start with a number higher than the previous head,
-- which indicates the number of `Tplt`s in the `BinTpltOrder`
-- that they are all "equally maximal" w/r/t.
-- (The order is partial, so they're not exactly equal,
-- but none is bigger than the others.)
-- The `fst`s do not have to increase consecutively.
-- For instance, if the only element of the list is `(0,as0)`,
-- and nothing in `as0` is involved in a relationship
-- that uses the first or second `Tplt` in the `BinTpltOrder`,
-- then the next pair to be pushed onto the front of the list
-- will have a `fst` greater than `2`.
