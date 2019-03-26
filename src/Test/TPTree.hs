{-# LANGUAGE ScopedTypeVariables #-}

module Test.TPTree where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Lens.Micro
import           Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as P

import           Util.PTree
import           Util.Direction


test_module_pTree :: T.Test
test_module_pTree = TestList [
    TestLabel "test_porestLeaf"     test_porestLeaf
  , TestLabel "test_focusedSubtree" test_focusedSubtree
  , TestLabel "test_focusedChild"   test_focusedChild
  , TestLabel "test_parentOfFocusedSubtree" test_parentOfFocusedSubtree
  , TestLabel "test_moveFocus" test_moveFocus
  ]

test_moveFocus :: T.Test
test_moveFocus = TestCase $ do
  let f  = pTreeLeaf (1 :: Int)
      t  = f   { _pTreeHasFocus = True }
      f_t = f  { _pMTrees =                    P.fromList [t] }
      t_f = t  { _pMTrees =                    P.fromList [f] }
      f_ft = f { _pMTrees = nextIfPossible <$> P.fromList [f,t] }
      f_tf = f { _pMTrees =                    P.fromList [t,f] }

  assertBool "Next"             $ moveFocusInPTree DirNext f_tf == f_ft
  assertBool "Next maxed out 1" $ moveFocusInPTree DirNext f_ft == f_ft

  assertBool "Prev maxed out 1" $ moveFocusInPTree DirPrev f_tf == f_tf
  assertBool "Prev"             $ moveFocusInPTree DirPrev f_ft == f_tf

  assertBool "Down maxed out 1" $ moveFocusInPTree DirDown f_t == f_t
  assertBool "Down"             $ moveFocusInPTree DirDown t_f == f_t

  assertBool "Up maxed out 1"   $ moveFocusInPTree DirUp t     == t
  assertBool "Up maxed out 2"   $ moveFocusInPTree DirUp f     == t
  assertBool "Up maxed out 3"   $ moveFocusInPTree DirUp t_f   == t_f
  assertBool "Up"               $ moveFocusInPTree DirUp f_t   == t_f

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

test_consUnderAndFocus :: T.Test
test_consUnderAndFocus = TestCase $ do
  let f    = pTreeLeaf (1 :: Int)
      t    = f { _pTreeHasFocus = True }
      f_t  = f { _pMTrees = P.fromList   [t] }
      f_ft = f { _pMTrees = P.fromList [f,t] }
  assertBool "1" $ consUnderAndFocus t f   == f_t
  assertBool "2" $ consUnderAndFocus t f_t == f_ft

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

