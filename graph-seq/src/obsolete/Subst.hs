-- not necessarily useless!
-- reconcileCondElts proved useful -- I moved it here, then back out.


-- | = Using `Subst` to restrict `CondElts`

-- | `restrictCondElts ss ces` takes the simple union of the results of
--  calling `restrictCondElts ss ces` for every s in ss.
restrictCondElts :: Set Subst -> CondElts -> CondElts
restrictCondElts ss ces = M.unionsWith S.union
                         $ S.map (flip restrictCondElts1 ces) ss

-- | It is as if `restrictCondElts1 s ce` first restricts ce to those Substs
-- reconcilable with s, and then replaces each with its reconciliation.
restrictCondElts1 :: Subst -> CondElts -> CondElts
restrictCondElts1 s ce = M.filter (not . null) reconciled
  where reconciled = M.map (reconcile1ToMany s) ce

  , TestLabel "testRestrictCondElts1" testRestrictCondElts1
  , TestLabel "testRestrictCondElts" testRestrictCondElts

testRestrictCondElts = TestCase $ do
  let (x,y,z) = (Var "x",Var "y",Var"z")
      x1    = M.fromList [ (x,1)                  ] :: Subst
      y11   = M.fromList [        (y,11)          ] :: Subst
      x1y11 = M.fromList [ (x,1), (y,11)          ] :: Subst
      xyz   = M.fromList [ (x,1), (y,11), (z,111) ] :: Subst
      x2y11 = M.fromList [ (x,2), (y,11)          ] :: Subst
      y12   = M.fromList [        (y,12)          ] :: Subst
      ces   = M.fromList [ (1, S.fromList [ x1y11
                                          , xyz ] )
                         , (2, S.singleton x2y11  ) ]
      ces'  = M.fromList [ (1, S.singleton x1)
                         , (2, S.singleton y12) ]

  assertBool "2" $ restrictCondElts (S.singleton y11) ces'
         == M.fromList [ (1, S.fromList [ x1y11 ] ) ]
  assertBool "1" $ restrictCondElts (S.singleton x1y11) ces
         == M.fromList [ (1, S.fromList [ x1y11
                                        , xyz ] ) ]
  assertBool "0" $ restrictCondElts (S.singleton y12) ces
         == M.empty

testRestrictCondElts1 = TestCase $ do
  let (x,y,z) = (Var "x",Var "y",Var"z")
      subst = M.fromList [ (x,1), (y,11) ]
      ces = M.fromList [ (1, S.fromList [ subst
                                        , M.insert z 111 subst ] )
                       , (2, S.fromList [ M.insert x 2 subst ] ) ]
  assertBool "1" $ restrictCondElts1 subst ces
         == M.fromList [ (1, S.fromList [ subst
                                        , M.insert z 111 subst ] ) ]
  -- TODO speed|memory : Notice how the z-binding is kept, whether or not
  -- it is relevant. If z is not used later, then maybe it should not be
  -- kept. This would prevent treating the Subst with the z-binding as
  -- distinct from the Subst without it, hence avoid duplicating some work.


-- | = Changing the names in a CondElts

-- | `recordDependencies vf@(Var name _) ce` replaces each instance
-- of `Var name mempty` in `ce` with `vf`.
recordDependencies :: Var -> CondElts -> CondElts
recordDependencies vf ce = let
  replace :: Subst -> Subst
  replace s = let mlk = M.lookup (unCondition vf) s in case mlk of
    Nothing -> s -- TODO ? Throw an error?
    Just lk -> M.insert vf lk $ M.delete (unCondition vf) s
  in M.map (S.map replace) ce

  , TestLabel "testRecordDependencies" testRecordDependencies
testRecordDependencies = TestCase $ do
  let [a,b,c,x] = map (\x -> Var x S.empty) ["a","b","c","x"]
      aOf_x = Var "a" $ S.singleton x
      s = S.fromList  [ M.fromList [ (a,1)      , (b,1)  ]
                      , M.fromList [ (a,2)      , (b,2)  ] ]
      t = S.fromList  [ M.fromList [ (a,11)     , (c,11) ]
                      , M.fromList [ (a,12)     , (c,12) ] ]
      s' = S.fromList [ M.fromList [ (aOf_x,1)  , (b,1)  ]
                      , M.fromList [ (aOf_x,2)  , (b,2)  ] ]
      t' = S.fromList [ M.fromList [ (aOf_x,11) , (c,11) ]
                      , M.fromList [ (aOf_x,12) , (c,12) ] ]
  assertBool "1" $             M.fromList [ (5,s'), (6,t') ] ==
    recordDependencies aOf_x ( M.fromList [ (5,s),  (6,t ) ] )



-- | = (a section that remains)
-- | = Building a `CondElts` from `Subst`s

-- | If each s in ss is a `Set Subst` derived from a different determinant**
-- of v, then `setSetSubstToCondElts v ss` creates a `CondElts` for each s,
-- and then reconciles the results.

setSetSubstToCondElts :: Var -> Set (Set Subst) -> Maybe CondElts
setSetSubstToCondElts v ss = reconcileCondElts condEltsPerDet where
  condEltsPerDet = S.map (setSubstToCondElts v) ss :: Set CondElts

  , TestLabel "testSetSetSubstToCondElts" testSetSetSubstToCondElts

testSetSetSubstToCondElts = TestCase $ do
  let (a,b,c,x) = (Var "a",Var "b",Var "c",Var "x")
      ss = S.fromList [ M.fromList [(x,1), (a,1), (b,1)      ]
                      , M.fromList [(x,1), (a,1),       (c,1)] ] :: Set Subst
      st = S.fromList [ M.fromList [(x,1), (a,1), (b,1)      ]
                      , M.fromList [(x,2), (a,1),       (c,1)] ] :: Set Subst
      sq = S.fromList [ M.fromList [(x,1), (a,1),       (c,1) ]
                      , M.fromList [(x,2), (a,1), (b,11)      ]
                      , M.fromList [(x,3), (a,1), (b,1)       ] ] :: Set Subst

  assertBool "3" $ setSetSubstToCondElts x (S.fromList [st,sq]) == Just
    ( M.fromList [ (1, S.singleton $ M.fromList [(a,1), (b,1),  (c,1)] )
                 , (2, S.singleton $ M.fromList [(a,1), (b,11), (c,1)] ) ] )
  assertBool "2" $ setSetSubstToCondElts x (S.fromList [ss,st]) ==
    Just ( M.singleton 1 $ S.fromList [ M.fromList [(a,1), (b,1), (c,1)]
                                      , M.fromList [(a,1), (b,1)       ] ] )
  assertBool "1" $ setSetSubstToCondElts x (S.singleton ss) ==
    Just ( M.singleton 1 $ S.fromList [ M.fromList [(a,1), (b,1)      ]
                                      , M.fromList [(a,1),        (c,1)] ] )

-- | This is a portion of a leading comment that I deleted, but
-- I kept the function it comments.
-- ASSUMES that the `Subst`s all came
-- from the same `CondElts`. Hence, none of them have to be
-- reconciled. Contrast this to `setSetSubstToCondElts`, in which
-- the results from each of the innser sets must be reconciled against
-- each other, because each outer set corresponds to a separate determinant.

setSubstToCondElts :: Var -> Set Subst -> CondElts
...
