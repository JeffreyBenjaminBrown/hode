{-# LANGUAGE ScopedTypeVariables
, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hode.Test.PTree.TPInitial where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Lens.Micro
import qualified Data.List.PointedList as P

import Hode.PTree.Initial


test_module_pTree_initial :: T.Test
test_module_pTree_initial = TestList [
    TestLabel "test_porestLeaf"     test_porestLeaf
  , TestLabel "test_focusedSubtree" test_focusedSubtree
  , TestLabel "test_focusedChild"   test_focusedChild
  , TestLabel "test_parentOfFocusedSubtree" test_parentOfFocusedSubtree
  , TestLabel "test_nudgeFocus_inPTree" test_nudgeFocus_inPTree
  , TestLabel "test_nudgeFocus_inPorest" test_nudgeFocus_inPorest
  , TestLabel "test_pListLenses" test_pListLenses
  , TestLabel "test_map" test_map
  , TestLabel "test_fold" test_fold
  , TestLabel "test_nudge" test_nudge
  , TestLabel "test_nudgeInPTree" test_nudgeInPTree
  , TestLabel "test_delete" test_delete
  , TestLabel "test_filterPList" test_filterPList
  , TestLabel "test_insertLeft_noFocusChange" test_insertLeft_noFocusChange
  ]

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
    nudgeInPTree DirPrev topFocused == topFocused &&
    nudgeInPTree DirNext topFocused == topFocused

  let midFocused = PTree 0 False $ Just $ P.PointedList
        [ PTree 1 False Nothing ]
        ( PTree 2 True  Nothing )
        [ PTree 3 False Nothing ]
  assertBool "prev" $
    nudgeInPTree DirPrev midFocused ==
    PTree 0 False ( Just $ P.PointedList []
                    ( PTree 2 True  Nothing ) -- focused
                    [ PTree 1 False Nothing
                    , PTree 3 False Nothing ] )
  assertBool "next" $
    nudgeInPTree DirNext midFocused ==
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

test_fold :: T.Test
test_fold = TestCase $ do
  assertBool "foldr" $ foldr (:) []
    ( PTree 3 True $
      P.fromList [ PTree 2 True Nothing
                 , PTree 1 True Nothing] )
    == [3,2,1]
  assertBool "foldl" $ foldl (flip (:)) []
    ( PTree 3 True $
      P.fromList [ PTree 2 True Nothing
                 , PTree 1 True Nothing] )
    == [1,2,3]
  assertBool "Foldable already defines maximum" $ maximum
    ( PTree 1 True $
      P.fromList [ PTree 2 True Nothing
                 , PTree 3 True Nothing] )
    == 3
  assertBool "Make sure order doesn't matter" $ maximum
    ( PTree 3 True $
      P.fromList [ PTree 2 True Nothing
                 , PTree 1 True Nothing] )
    == 3

  assertBool "maximum of each PTree in a Porest" $
    fmap (fmap maximum)
    ( P.fromList [ PTree 1 True $
                   P.fromList [ PTree 2 True Nothing,
                                PTree 3 True Nothing ]
                 , PTree 4 True Nothing ]
      :: Maybe (Porest Int) )
    == P.fromList [3,4]

  assertBool "maximum in a Porest" $
    fmap -- into the Maybe
    ( maximum . -- maximum across the PTrees
      fmap maximum ) -- maximum within each PTree
    ( P.fromList [ PTree 1 True $
                   P.fromList [ PTree 2 True Nothing,
                                PTree 3 True Nothing ]
                 , PTree 4 True Nothing ]
      :: Maybe (Porest Int) )
    == Just 4

test_map :: T.Test
test_map = TestCase $ do
  assertBool "flat PTree, changing type" $ fmap (,"a")
    (PTree 1 True Nothing) ==
    PTree (1,"a") True Nothing
  assertBool "2-level PTree" $ fmap (+1)
    (PTree 1 True $ P.fromList [ PTree 2 True Nothing
                               , PTree 3 True Nothing]) ==
    (PTree 2 True $ P.fromList [ PTree 3 True Nothing
                               , PTree 4 True Nothing])

  assertBool "2-level PTree changing type" $ fmap (Right . (+1))
    ( PTree 1 True $ P.fromList [ PTree 2 True Nothing
                                , PTree 3 True Nothing]) ==
    ( PTree (Right 2 :: Either String Int)
      True $ P.fromList [ PTree (Right 3) True Nothing
                        , PTree (Right 4) True Nothing])

  assertBool "flat Porest" $
    fmap ( fmap ( fmap (+1) ) )
      -- 3 fmaps: the Maybe, the PointedList, each PTree
    ( P.fromList [ PTree 1 True Nothing
                 , PTree 2 True Nothing ]
      :: Maybe (Porest Int) ) ==
    ( P.fromList [ PTree 2 True Nothing
                 , PTree 3 True Nothing ] )

  assertBool "2-level Porest" $
    fmap ( fmap ( fmap (+1) ) )
      -- 3 fmaps: the Maybe, the PointedList, each PTree
    ( P.fromList [ PTree 1 True $
                   P.fromList [ PTree 2 True Nothing,
                                PTree 3 True Nothing ]
                 , PTree 4 True Nothing ]
      :: Maybe (Porest Int) ) ==
    ( P.fromList [ PTree 2 True $
                   P.fromList [ PTree 3 True Nothing,
                                PTree 4 True Nothing ]
                 , PTree 5 True Nothing ] )

test_pListLenses :: T.Test
test_pListLenses = TestCase $ do
  let toPList = maybe
        (error "impossible: P.fromList fails only on []")
        id . P.fromList
      l  = [1..3 :: Int]
      l' = [4..6 :: Int]
      pl  = toPList l
      pl' = toPList l'
  assertBool "get"         $  pl ^. getPList        == l
  assertBool "set"         $ (pl &  setPList .~ l') == pl'
  assertBool "fail to set" $ (pl &  setPList .~ []) == pl

test_nudgeFocus_inPorest :: T.Test
test_nudgeFocus_inPorest = TestCase $ do
  let -- In these names, u=up, d=down, and otherwise n=next is implicit
    pList :: [a] -> P.PointedList a
    pList = maybe (error "impossible unless given [].") id . P.fromList
    nip   = nextIfPossible
    f     = pTreeLeaf (1 :: Int)
    t     = f { _pTreeHasFocus = True }
    _f_nt = nip $ pList [f,t]
    _t_nf =       pList [t,f]

  assertBool "1" $ nudgeFocus_inPorest DirNext _t_nf
                                          == _f_nt
  assertBool "2" $ nudgeFocus_inPorest DirNext _f_nt
                                          == _f_nt
  assertBool "1" $ nudgeFocus_inPorest DirPrev _t_nf
                                          == _t_nf
  assertBool "2" $ nudgeFocus_inPorest DirPrev _f_nt
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

  assertBool "Next"             $ nudgeFocus_inPTree DirNext f_dt_f == f_df_t
  assertBool "Next maxed out 1" $ nudgeFocus_inPTree DirNext f_df_t == f_df_t

  assertBool "Prev maxed out 1" $ nudgeFocus_inPTree DirPrev f_dt_f == f_dt_f
  assertBool "Prev"             $ nudgeFocus_inPTree DirPrev f_df_t == f_dt_f

  assertBool "Down maxed out 1" $ nudgeFocus_inPTree DirDown f_dt == f_dt
  assertBool "Down"             $ nudgeFocus_inPTree DirDown t_df == f_dt
  assertBool "Down from middle" $ nudgeFocus_inPTree DirDown f_dt_df_uf
                                                        == f_df_dt_uf

  assertBool "Up maxed out 1"   $ nudgeFocus_inPTree DirUp t    == t
  assertBool "Up maxed out 2"   $ nudgeFocus_inPTree DirUp f    == t
  assertBool "Up maxed out 3"   $ nudgeFocus_inPTree DirUp t_df == t_df
  assertBool "Up"               $ nudgeFocus_inPTree DirUp f_dt == t_df
  assertBool "Up from bottom of 3 layers"
                                $ nudgeFocus_inPTree DirUp f_df_dt_uf
                                                      == f_dt_df_uf

test_porestLeaf :: T.Test
test_porestLeaf = TestCase $ do
  assertBool "1" $ Just (porestLeaf 1) ==
    ( P.fromList [ PTree { _pTreeLabel = 1 :: Int
                         , _pTreeHasFocus = False
                         , _pMTrees = Nothing } ] )

test_parentOfFocusedSubtree :: T.Test
test_parentOfFocusedSubtree = TestCase $ do
  let f  = pTreeLeaf (1 :: Int)
      t  = f { _pTreeHasFocus = True }
      ft = f { _pMTrees = Just $ P.singleton t }
      ff = f { _pMTrees = Just $ P.singleton f }
      tf = t { _pMTrees = Just $ P.singleton f }

  assertBool "s1" $ (setParentOfFocusedSubtree .~ f) t  == t
  assertBool "s2" $ (setParentOfFocusedSubtree .~ t) f  == f
  assertBool "s3" $ (setParentOfFocusedSubtree .~ t) tf == tf
  assertBool "s4" $ (setParentOfFocusedSubtree .~ t) ft == t

  assertBool "g1" $ (t  ^. getParentOfFocusedSubtree) == Nothing
  assertBool "g2" $ (f  ^. getParentOfFocusedSubtree) == Nothing
  assertBool "g3" $ (ft ^. getParentOfFocusedSubtree) == Just ft
  assertBool "g4" $ (ff ^. getParentOfFocusedSubtree) == Nothing
  assertBool "g4" $ (tf ^. getParentOfFocusedSubtree) == Nothing

test_consUnder_andFocus :: T.Test
test_consUnder_andFocus = TestCase $ do
  let f    = pTreeLeaf (1 :: Int)
      t    = f { _pTreeHasFocus = True }
      f_t  = f { _pMTrees = P.fromList   [t] }
      f_ft = f { _pMTrees = P.fromList [f,t] }
  assertBool "1" $ consUnder_andFocus t f   == f_t
  assertBool "2" $ consUnder_andFocus t t   == f_t
  assertBool "3" $ consUnder_andFocus f t   == f_t
  assertBool "4" $ consUnder_andFocus t f_t == f_ft

test_focusedSubtree :: T.Test
test_focusedSubtree = TestCase $ do
  let f  = pTreeLeaf (1 :: Int)
      t  = f { _pTreeHasFocus = True }
      ft = f { _pMTrees = Just $ P.singleton t }
      ff = f { _pMTrees = Just $ P.singleton f }
      tf = t { _pMTrees = Just $ P.singleton f }

  assertBool "s1" $ (f & setFocusedSubtree . pTreeLabel %~ (+1))
    == f
  assertBool "s2" $ (setFocusedSubtree . pTreeLabel %~ (+1)) t
    == (pTreeLabel .~ 2) t
  assertBool "s3" $ (setFocusedSubtree . pTreeLabel %~ (+1)) tf
    == (pTreeLabel .~ 2) tf
  assertBool "s4" $ (setFocusedSubtree . pTreeLabel %~ (+1)) ft
    == (pMTrees . _Just . P.focus . pTreeLabel .~ 2) ft

  assertBool "1" $ f  ^. getFocusedSubtree == Nothing
  assertBool "2" $ t  ^. getFocusedSubtree == Just t
  assertBool "3" $ ft ^. getFocusedSubtree == Just t
  assertBool "4" $ ff ^. getFocusedSubtree == Nothing
  assertBool "5" $ tf ^. getFocusedSubtree == Just tf

test_focusedChild :: T.Test -- :: PTree a -> Maybe (PTree a)
test_focusedChild = TestCase $ do
  let f       = pTreeLeaf (1 :: Int)
      t       = f { _pTreeHasFocus = True }
      f_t     = f { _pMTrees = P.fromList [t] }
      t_f     = t { _pMTrees = P.fromList [f] }
      f_ft_tf = f { _pMTrees = P.fromList [f_t,t_f] }

  assertBool "1" $ f       ^. getFocusedChild == Nothing
  assertBool "2" $ t       ^. getFocusedChild == Nothing
  assertBool "3" $ f_t     ^. getFocusedChild == Just t
  assertBool "3" $ t_f     ^. getFocusedChild == Nothing
  assertBool "4" $ f_ft_tf ^. getFocusedChild == Just t_f
