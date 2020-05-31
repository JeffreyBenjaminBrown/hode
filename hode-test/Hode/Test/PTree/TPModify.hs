{-# LANGUAGE ScopedTypeVariables
, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hode.Test.PTree.TPModify where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Control.Lens
import qualified Data.List.PointedList          as P
import           Data.Maybe

import Hode.PTree.Initial
import Hode.PTree.Modify


test_module_pTree_modify :: T.Test
test_module_pTree_modify = TestList [
    TestLabel "test_nudgeFocus_inPTree" test_nudgeFocus_inPTree
  , TestLabel "test_nudgeFocus_inPorest" test_nudgeFocus_inPorest
  , TestLabel "test_nudge" test_nudge
  , TestLabel "test_nudgeInPTree" test_nudgeInPTree
  , TestLabel "test_delete" test_delete
  , TestLabel "test_filterPList" test_filterPList
  , TestLabel "test_insertLeft_noFocusChange" test_insertLeft_noFocusChange
  , TestLabel "test_sortPList_asList" test_sortPList_asList
  , TestLabel "test_insertLeaf_next" test_insertLeaf_next
  ]

test_insertLeaf_next :: T.Test
test_insertLeaf_next = TestCase $ do
  let l :: Int -> PTree Int
      l i = PTree i False Nothing -- leaf, no focus
      f1 :: Porest Int = P.PointedList [] (l 1) []
                         & P.focus . pTreeHasFocus .~ True
      f2 :: Porest Int = P.PointedList [l 1] (l 2) []
                         & P.focus . pTreeHasFocus .~ True
  assertBool "a flat forest" $ insertLeaf_next 2 f1 == f2

  let g2 :: Porest Int = P.PointedList [] t []
        where t = PTree 0 False $ Just -- tree, focused on 2
                  $ P.PointedList [ PTree 1 False Nothing ]
                                  ( PTree 2 True  Nothing )
                                  [ PTree 3 False Nothing ]
      g22 :: Porest Int = P.PointedList [] t []
        where t = PTree 0 False $ Just -- tree, focused on 22
                  $ P.PointedList [ PTree 2  False Nothing
                                  , PTree 1  False Nothing ]
                                  ( PTree 22 True  Nothing )
                                  [ PTree 3  False Nothing ]
  assertBool "a depth-2 forest" $ insertLeaf_next 22 g2 == g22

test_sortPList_asList :: T.Test
test_sortPList_asList = TestCase $ do
  let as :: P.PointedList ((),Int) =
        fromJust $ P.fromList
        $ fmap ((),) [0..6]
      bs :: [Int] = [4,2,6,0]
  assertBool "" $
    sortPList_asList snd bs as
    == fmap ((),) ( P.PointedList
                    [6,2,4] -- pitfall: reversed
                    0 [1,3,5] )

test_insertLeft_noFocusChange :: T.Test
test_insertLeft_noFocusChange = TestCase $ do
  assertBool "works on the left side" $
    (insertLeft_noFocusChange 0 $ P.PointedList [] 1 [2..4])
    == P.PointedList [0] 1 [2..4]
  assertBool "works on the right side" $
    (insertLeft_noFocusChange 1 $ P.PointedList [2..4] 0 [])
    == P.PointedList [1..4] 0 []

test_filterPList :: T.Test
test_filterPList = TestCase $ do
  let left = P.PointedList [] 3 [4,5]
      middle = P.PointedList [2] 3 [4]
      right = P.PointedList [3,2] 4 []
  assertBool "id left"   $ filterPList (/= 1) left   == Just left
  assertBool "id middle" $ filterPList (/= 1) middle == Just middle
  assertBool "id right"  $ filterPList (/= 1) right  == Just right

  assertBool "drop focus at left" $
    filterPList (/= 1) (P.PointedList [] 1 [4,5])
    == Just (P.PointedList [] 4 [5])
  assertBool "drop focus in middle" $
    filterPList (/= 3) (P.PointedList [2,1] 3 [4,5])
    == Just (P.PointedList [] 1 [2,4,5])
  assertBool "drop focus at right" $
    filterPList (/= 3) (P.PointedList [2,1] 3 [])
    == Just (P.PointedList [] 1 [2])

  assertBool "drop from right with focus at left" $
    filterPList (/= 1) (P.PointedList [] 4 [2,1,5])
    == Just (P.PointedList [] 4 [2,5])
  assertBool "drop from sides with focus in middle" $
    filterPList (/= 1) (P.PointedList [2,1] 3 [4,5,1])
    == Just (P.PointedList [2] 3 [4,5])
  assertBool "drop from left with focus at right" $
    filterPList (/= 1) (P.PointedList [1,2,1,3] 4 [])
    == Just (P.PointedList [2,3] 4 [])

test_delete :: T.Test
test_delete = TestCase $ do

  let tn = PTree 1 False $ -- Tree, no focus
           P.fromList [ PTree 2 False Nothing
                      , PTree 3 False Nothing ]
      t1 = PTree 1 True $ -- Tree, focus at top
           P.fromList [ PTree 2 False Nothing
                      , PTree 3 False Nothing ]
      t2 = PTree 1 False $ -- Tree, focus on 2
           P.fromList [ PTree 2 True Nothing
                      , PTree 3 False Nothing ]
      t3 = PTree 1 False $ -- Tree, focus on 3, 2 absent
           P.fromList [ PTree 3 True Nothing ]

  assertBool "can't delete top of tree" $
    deleteInPTree tn == tn
  assertBool "delete 2, left focused on 3" $
    deleteInPTree t2 == t3

  assertBool "tn becomes t1 and replaces t2" $
    deleteInPorest (P.PointedList [] t1 [tn, t3])
    == Just (P.PointedList [] t1 [t3])
  assertBool "invalid input (multiple focus)" $
    deleteInPorest (P.PointedList [] t1 [t1, t3])
    == Just (P.PointedList [] t1 [t3])
  assertBool "Replaces from left when possible" $
    deleteInPorest (P.PointedList [tn] t1 [t3])
    == Just (P.PointedList [] t1 [t3])


test_nudgeInPTree :: T.Test
test_nudgeInPTree = TestCase $ do
  let topFocused = PTree 3 True $
          P.fromList [ PTree 2 False Nothing
                     , PTree 1 False Nothing]
  assertBool "top" $
    nudgeInPTree ToPrev topFocused == topFocused &&
    nudgeInPTree ToNext topFocused == topFocused

  let midFocused = PTree 0 False $ Just $ P.PointedList
        [ PTree 1 False Nothing ]
        ( PTree 2 True  Nothing )
        [ PTree 3 False Nothing ]
  assertBool "prev" $
    nudgeInPTree ToPrev midFocused ==
    PTree 0 False ( Just $ P.PointedList []
                    ( PTree 2 True  Nothing ) -- focused
                    [ PTree 1 False Nothing
                    , PTree 3 False Nothing ] )
  assertBool "next" $
    nudgeInPTree ToNext midFocused ==
    PTree 0 False ( Just $ P.PointedList
                    [ PTree 3 False Nothing
                    , PTree 1 False Nothing ]
                    ( PTree 2 True  Nothing ) -- focused
                    [] )

test_nudge :: T.Test
test_nudge = TestCase $ do
  -- PITFALL: For efficiency, a `PointedList`'s first list appears reversed.
  -- Thus the numbers [1..4] appear here in order.
  let pl =                        P.PointedList [2,1] 0 [3,4]
  assertBool "" $ nudgePrev pl == P.PointedList [1] 0 [2,3,4]
  assertBool "" $ nudgeNext pl == P.PointedList [3,2,1] 0 [4]

test_nudgeFocus_inPorest :: T.Test
test_nudgeFocus_inPorest = TestCase $ do
  let -- In these names, u=up, d=down, and otherwise n=next is implicit
    pList :: [a] -> P.PointedList a
    pList = maybe (error "impossible unless given [].") id . P.fromList
    f     = pTreeLeaf (1 :: Int)
    t     = f { _pTreeHasFocus = True }
    _f_nt = nextIfPossible $ pList [f,t]
    _t_nf =                  pList [t,f]

  assertBool "1" $ nudgeFocus_inPorest ToNext _t_nf
                                          == _f_nt
  assertBool "2" $ nudgeFocus_inPorest ToNext _f_nt
                                          == _f_nt
  assertBool "1" $ nudgeFocus_inPorest ToPrev _t_nf
                                          == _t_nf
  assertBool "2" $ nudgeFocus_inPorest ToPrev _f_nt
                                          == _t_nf

test_nudgeFocus_inPTree :: T.Test
test_nudgeFocus_inPTree = TestCase $ do
  let -- In these names, u=up, d=down, and otherwise n=next is implicit
    f          = pTreeLeaf (1 :: Int)
    t          = f { _pTreeHasFocus = True }
    f_dt       = f { _pMTrees =                    P.fromList [t] }
    t_df       = t { _pMTrees =                    P.fromList [f] }
    f_df_t     = f { _pMTrees = nextIfPossible <$> P.fromList [f,t] }
    f_dt_f     = f { _pMTrees =                    P.fromList [t,f] }
    f_dt_df_uf = f { _pMTrees =                    P.fromList [t_df,f] }
    f_df_dt_uf = f { _pMTrees =                    P.fromList [f_dt,f] }

  assertBool "Next"             $ nudgeFocus_inPTree ToNext f_dt_f == f_df_t
  assertBool "Next maxed out 1" $ nudgeFocus_inPTree ToNext f_df_t == f_df_t

  assertBool "Prev maxed out 1" $ nudgeFocus_inPTree ToPrev f_dt_f == f_dt_f
  assertBool "Prev"             $ nudgeFocus_inPTree ToPrev f_df_t == f_dt_f

  assertBool "Down maxed out 1" $ nudgeFocus_inPTree ToLeaf f_dt == f_dt
  assertBool "Down"             $ nudgeFocus_inPTree ToLeaf t_df == f_dt
  assertBool "Down from middle" $ nudgeFocus_inPTree ToLeaf f_dt_df_uf
                                                        == f_df_dt_uf

  assertBool "Up maxed out 1"   $ nudgeFocus_inPTree ToRoot t    == t
  assertBool "Up maxed out 2"   $ nudgeFocus_inPTree ToRoot f    == t
  assertBool "Up maxed out 3"   $ nudgeFocus_inPTree ToRoot t_df == t_df
  assertBool "Up"               $ nudgeFocus_inPTree ToRoot f_dt == t_df
  assertBool "Up from bottom of 3 layers"
                                $ nudgeFocus_inPTree ToRoot f_df_dt_uf
                                                      == f_dt_df_uf
