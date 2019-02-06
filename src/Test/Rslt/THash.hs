{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.THash where

import           Prelude hiding (lookup)
import           Data.Either
import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Rslt.Edit
import Rslt.RTypes
import Rslt.Lookup
import qualified Test.Rslt.RData as D
import Util


test_module_rslt_hash = TestList [
    TestLabel "test_subExprs" test_subExprs
  , TestLabel "test_hLookup" test_hLookup
  ]

test_hLookup = TestCase $ do
  -- Still untested:
    -- HEval paths of length > 1
    -- One HEval inside another

  assertBool "find 2" $ hLookup D.big
    ( HExpr $ ExprAddr 2)
    == Right ( S.fromList [2] )

  assertBool "9 is the only Expr with 2 as member 1"
    $ hLookup D.big
    ( HMap $ M.fromList [ ( RoleMember 1, HExpr $ ExprAddr 2) ] )
    == Right ( S.fromList [9] )

  assertBool "nothing has 10 as its first member."
    $ hLookup D.big
    ( HMap $ M.fromList [ ( RoleMember 1, HExpr $ ExprAddr 10) ] )
    == Right S.empty

  assertBool "2 is the first member of the only thing (9) that has 2 as its first member. (Duh.)"
    $ hLookup D.big ( HEval
                    ( M.fromList [ (RoleMember 1, HExpr $ ExprAddr 2) ] )
                    [ [ RoleMember 1 ] ] )
    == Right ( S.fromList [2] )

  assertBool "9 is the only Expr in D.big whose 2nd member is 3."
    $ hLookup D.big ( HMap ( M.singleton (RoleMember 2)
                           $ HExpr $ Word "3" ) )
    == Right ( S.fromList [9] )

  assertBool "2 is the first member of the only thing (9) that has 3 as its second member."
    $ hLookup D.big ( HEval
                    ( M.fromList [ (RoleMember 2, HExpr $ ExprAddr 3) ] )
                    [ [ RoleMember 1 ] ] )
    == Right ( S.fromList [2] )

  assertBool "9 is the only thing whose first member is 2. The HEval returns 2. 7 is the only thing whose second member is equal to what that HEval returned, so 7 is what the outer HMap returns." $ hLookup D.big
    ( HMap ( M.singleton (RoleMember 2)
             $ HEval
             ( M.fromList [ (RoleMember 1, HExpr $ ExprAddr 2) ] )
             [ [ RoleMember 1 ] ] ) )
    == Right ( S.fromList [7] )

test_subExprs = TestCase $ do
  let r = fromRight (error "wut") $ insertAt 7 (Rel' [5,5] 4) D.rslt
  assertBool "1" $ subExprs r [ [RoleMember 1, RoleMember 1] ] 7
    == Right (S.fromList [1])
  assertBool "2" $ subExprs r [ [RoleMember 2, RoleMember 2] ] 7
    == Right (S.fromList [2])
  assertBool "3" $ subExprs r [ [RoleMember 2, RoleMember 1] ] 7
    == Right (S.fromList [1])
  assertBool "3" $ subExprs r [ [RoleMember 2, RoleMember 1]
                                 , [RoleMember 1]
                                 ] 7
    == Right (S.fromList [1,5])
  assertBool "4" $ subExprs r [ [RoleMember 2, RoleMember 1]
                                 , [RoleMember 1, RoleMember 1]
                                 ] 7
    == Right (S.fromList [1])
  assertBool "5" $ subExprs r [ [RoleMember 1, RoleMember 1]
                                 , [RoleMember 1, RoleMember 2]
                                 , [RoleMember 2, RoleMember 2]
                                 ] 7
    == Right (S.fromList [1,2])
