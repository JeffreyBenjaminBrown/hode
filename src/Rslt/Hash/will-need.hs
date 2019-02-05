-- | It used to be that HMap was a map from Role to (Either HIt HExpr).
-- This would return a list of every path to an HIts in an HMap.
-- When I write a parser for HExprs, I will need to do something similar.

pathsToIts :: HMap -> [[Role]]
pathsToIts hm = x3 where

  go :: Either HIt HExpr -> [[Role]]
  go (Left HIt)        = [[]] -- This lists the unique
    -- way to descend to an `HIt` from here, which is to stay still.
  go (Right (HMap hm)) = pathsToIts hm
  go (Right _)         = [] -- Empty: One cannot descend to an HIt from here.

  (x1 :: Map Role [[Role]]) = M.map go hm
  (x2 :: Map Role [[Role]]) = M.mapWithKey (\k a -> map (k:) a) x1
  (x3 ::          [[Role]]) = concat $ M.elems x2

[ ...
  TestLabel "test_pathsToIts" test_pathsToIts
...]

test_pathsToIts = TestCase $ do
  assertBool "1" $ let
    x = M.fromList
      [ ( RoleMember 1
        , Right $ HMap $ M.fromList [ ( RoleMember 2, Left HIt ) ] )
      , ( RoleMember 3
        , Right $ HAnd $ error "irrelevant" ) ]
    in pathsToIts x == [ [ RoleMember 1, RoleMember 2 ] ]
