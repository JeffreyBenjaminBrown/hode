{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TNoUI where

import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Test.HUnit

import Hode.NoUI
import Hode.Rslt.Binary
import Hode.Rslt.Index
import Hode.Rslt.RTypes


test_module_NoUI :: Test
test_module_NoUI = TestList [
  TestLabel "test_insert" test_insert,
  TestLabel "test_nFindSort" test_nFindSort
  ]

test_nFindSort :: Test
test_nFindSort = TestCase $ do
  let Right rLine = nInserts (mkRslt mempty)
        -- These are intentionally out of order on the screen.
        -- (As numbers they are ordered normally.)
        [ "1 #lt 2", "1 #lt 3",
          "0 #lt 1", "0 #lt 2", -- omitted: 0 < 3
          "2 #lt 3" ]
      Right tplt_a  = head . S.toList <$>
                      nFindAddrs rLine "/t /_ lt /_"

  assertBool "" $
    ( (both %~ map snd) <$> nFindSort rLine
      (LeftEarlier,tplt_a)
      "/eval /it #lt /_" )
    == Right ( map (Phrase . show) [0,1,2::Int]
             , [] )

  assertBool "" $
    ( (both %~ map snd) <$> nFindSort rLine
      (LeftEarlier,tplt_a)
      "/eval /_ #lt /it" )
    == Right ( map (Phrase . show) [1,2,3::Int]
             , [] )

  assertBool "" $
    ( (both %~ map snd) <$> nFindSort rLine
      (RightEarlier,tplt_a)
      "/eval /it #lt /_" )
    == Right ( map (Phrase . show) [2,1,0::Int]
             , [] )

  assertBool "" $
    ( (both %~ map snd) <$> nFindSort rLine
      (RightEarlier,tplt_a)
      "/eval /_ #lt /it" )
    == Right ( map (Phrase . show) [3,2,1::Int]
             , [] )

test_insert :: Test
test_insert = TestCase $ do
  let ab = "a # b"
      cd = "c # d"
      Right (r1,a1,_) = nInsert (mkRslt mempty) ab
      Right (r2,a2,_) = nInsert r1 cd
      Right (s1,b1,_) = nInsert (mkRslt mempty) cd
      Right (s2,b2,_) = nInsert s1 ab
  assertBool "" $ a1 == b1
  assertBool "" $ a2 == b2
  assertBool "" $ r2 /= s2 -- they are isomorphic but not equal
  assertBool "" ( M.size (_addrToRefExpr r2) ==
                  M.size (_addrToRefExpr s2) )
  assertBool "" ( ( S.fromList $ M.elems $ _addrToRefExpr r2) ==
                  ( S.fromList $ M.elems $ _addrToRefExpr s2) )
  assertBool "" ( Right r2 ==
                  nInserts (mkRslt mempty) [ab,cd] )

-- -- It's hard to write a natural test of deletion,
-- -- because deleting a phrase at address X causes
-- -- (Phrase "") to move to that location.
-- Right abRel_cdRel = nInserts (mkRslt mempty)
--                     [ "a # b", "c # d" ]
-- Right abRel_cd =    nInserts (mkRslt mempty)
--                     [ "a # b", "c", "d" ]
-- > _addrToRefExpr <$> Right abRel_cdRel
-- (fromList [(0,Phrase' ""),(1,Tplt' (Tplt Nothing [0] Nothing)),
--            (2,Phrase' "a"),(3,Phrase' "b"),(4,Rel' (Rel [2,3] 1)),
--            (5,Phrase' "c"),(6,Phrase' "d"),(7,Rel' (Rel [5,6] 1))])
-- > _addrToRefExpr <$> nDelete abRel_cdRel "c # d"
-- (fromList [               (1,Tplt' (Tplt Nothing [7] Nothing)),
--             (2,Phrase' "a"),(3,Phrase' "b"),(4,Rel' (Rel [2,3] 1)),
--             (5,Phrase' "c"),(6,Phrase' "d"),(7,Phrase' "")])
-- > _addrToRefExpr <$> Right abRel_cd
-- (fromList [(0,Phrase' ""),(1,Tplt' (Tplt Nothing [0] Nothing)),
--            (2,Phrase' "a"),(3,Phrase' "b"),(4,Rel' (Rel [2,3] 1)),
--            (5,Phrase' "c"),(6,Phrase' "d")])
