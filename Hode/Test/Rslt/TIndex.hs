module Hode.Test.Rslt.TIndex where

import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import           Hode.Rslt.RTypes
import           Hode.Rslt.Index
import           Hode.Rslt.RValid
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_index :: Test
test_module_rslt_index = TestList [
    TestLabel "test_invertPositions" test_invertPositions
  , TestLabel "test_checkDb" test_checkDb
  , TestLabel "test_mkRslt" test_mkRslt
  ]

test_mkRslt :: Test
test_mkRslt = TestCase $ do
  assertBool "D.rslt" $ D.rslt_rightCapped
    == Rslt {
    _addrToRefExpr = M.fromList
      [(0,Phrase' "")
      ,(1,Phrase' "dog")
      ,(2,Phrase' "oxygen")
      ,(3,Phrase' "needs")
      ,(4,Tplt' (Tplt Nothing [3] $ Just 1))
      ,(5,Rel' (Rel [1,2] 4))
      ,(6,Rel' (Rel [5,2] 4))],
    _refExprToAddr = M.fromList
      [(Phrase' "",0)
      ,(Phrase' "dog",1)
      ,(Phrase' "needs",3)
      ,(Phrase' "oxygen",2)
      ,(Rel' (Rel [1,2] 4),5)
      ,(Rel' (Rel [5,2] 4),6)
      ,(Tplt' (Tplt Nothing [3] $ Just 1),4)],
    _variety = M.fromList
      [(0,(PhraseCtr,0))
      ,(1,(PhraseCtr,0))
      ,(2,(PhraseCtr,0))
      ,(3,(PhraseCtr,0))
      ,(4,(TpltCtr,2))
      ,(5,(RelCtr,2))
      ,(6,(RelCtr,2))],
    _has = M.fromList
      [(4,M.fromList [(RoleInTplt' $ RoleJoint 1,3)
                     ,(RoleInTplt' $ RoleCap CapRight,1)])
      ,(5,M.fromList [(RoleInRel' $ RoleTplt,4)
                     ,(RoleInRel' $ RoleMember 1,1)
                     ,(RoleInRel' $ RoleMember 2,2)])
      ,(6,M.fromList [(RoleInRel' $ RoleTplt,4)
                     ,(RoleInRel' $ RoleMember 1,5)
                     ,(RoleInRel' $ RoleMember 2,2)])],
    _isIn = M.fromList
      [(1,S.fromList [(RoleInRel' $ RoleMember 1,5)
                     ,(RoleInTplt' $ RoleCap CapRight,4)])
      ,(2,S.fromList [(RoleInRel' $ RoleMember 2,5)
                     ,(RoleInRel' $ RoleMember 2,6)])
      ,(3,S.fromList [(RoleInTplt' $ RoleJoint 1,4)])
      ,(4,S.fromList [(RoleInRel' $ RoleTplt,5)
                     ,(RoleInRel' $ RoleTplt,6)])
      ,(5,S.fromList [(RoleInRel' $ RoleMember 1,6)])]}

test_checkDb :: Test
test_checkDb = TestCase $ do
  assertBool "1" $ M.toList (relsWithoutMatchingTplts $ mkRslt D.badRefExprs)
    == [ (1001, Rel' $ Rel [1,2] 5)
       , (1002, Rel' $ Rel [1, 2] $ -1000) ]
  assertBool "2" $ M.toList (collectionsWithAbsentAddrs $ mkRslt D.badRefExprs)
    == [(1002, [-1000])]

test_invertPositions :: Test
test_invertPositions = TestCase $ do
  let ips = foldl invertAndAddPositions M.empty
        [ (1,  [ (RoleInRel' $ RoleMember 1, 11 )
               , (RoleInRel' $ RoleMember 2, 22 ) ] )
        , (11, [ (RoleInRel' $ RoleMember 1, 1  )
               , (RoleInRel' $ RoleMember 2, 22 ) ] )
        , (3,  [ (RoleInRel' $ RoleMember 1, 1  ) ] )
        ]
  assertBool "1" $ ips == M.fromList
    [(1,  S.fromList [(RoleInRel' $ RoleMember 1,3  )
                     ,(RoleInRel' $ RoleMember 1,11 )])
    ,(11, S.fromList [(RoleInRel' $ RoleMember 1,1  )])
    ,(22, S.fromList [(RoleInRel' $ RoleMember 2,1  )
                     ,(RoleInRel' $ RoleMember 2,11 )])]
