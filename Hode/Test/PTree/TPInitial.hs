{-# LANGUAGE ScopedTypeVariables
, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hode.Test.PTree.TPInitial where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Lens.Micro
import qualified Data.List.PointedList as P

import Hode.PTree.Initial
import Hode.PTree.Modify


test_module_pTree_initial :: T.Test
test_module_pTree_initial = TestList [
    TestLabel "test_porestLeaf"     test_porestLeaf
  , TestLabel "test_focusedSubtree" test_focusedSubtree
  , TestLabel "test_focusedChild"   test_focusedChild
  , TestLabel "test_parentOfFocusedSubtree" test_parentOfFocusedSubtree
  , TestLabel "test_pListLenses" test_pListLenses
  , TestLabel "test_fmap" test_fmap
  , TestLabel "test_fold" test_fold
  ]

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

test_fmap :: T.Test
test_fmap = TestCase $ do
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
