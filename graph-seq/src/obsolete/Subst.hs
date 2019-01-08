I *hope* this stuff is obsolete.


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


-- | = Reconciling `CondElts`s

-- | `reconcileCondEltsAtElt ces`returns the biggest `CondElts` possible
-- consistent with every `CondElt` in the input. That is, for every `Elt` e,
-- and every `Subst` s in `reconcileCondEltsAtElt ces`, and every
-- `CondElt` ce in ces, s is consistent with at least one `Subst` in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.
-- (Each determinant of a VarFunc implies a separate CondElts for it.)

reconcileCondElts :: Set CondElts -> Maybe CondElts
reconcileCondElts ces = if null u then Nothing else Just u where
   keys :: Set Elt
   keys = S.unions $ S.map M.keysSet ces
   reconciled :: Set CondElts
   reconciled = let f = flip reconcileCondEltsAtElt ces
     in S.map fromJust $ S.filter isJust $ S.map f keys
   u = M.unions reconciled :: CondElts

-- | `reconcileCondEltsAtElt e ces` returns the biggest `CondElts` possible
-- such that each value it associates with e is consistent with each of
-- the `CondElts`s in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.
-- (Each determinant of a VarFunc implies a separate CondElts for it.)

reconcileCondEltsAtElt :: Elt -> Set CondElts -> Maybe CondElts
reconcileCondEltsAtElt e ces = do
  let maybeConds = S.map (M.lookup e) ces :: Set (Maybe (Set Subst))
    -- the conditions under which `e` can obtain
  case S.member Nothing maybeConds of
    True -> Nothing
    False -> let conds = S.map fromJust maybeConds :: Set (Set Subst)
                 rConds = reconcile conds
      in if null rConds then Nothing
         else Just $ M.singleton e rConds

  , TestLabel "testReconcileCondEltsAtElt" testReconcileCondEltsAtElt
  , TestLabel "testReconcileCondElts" testReconcileCondElts

testReconcileCondElts = TestCase $ do
  let (a,b,c,x) = (Var "a",Var "b",Var "c",Var "x")
      ce, cf :: CondElts
      ce = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 1) ]
                                       , M.fromList [ (a, 2), (b, 2) ] ] )
                      , (2, S.fromList [ M.fromList [ (a, 1), (b, 1) ] ] )
                      , (3, S.fromList [ M.empty ] ) ]
      cf = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 2) ]
                                       , M.fromList [ (a, 2), (b, 2) ] ] )
                      , (2, S.fromList [ M.fromList [ (a, 1), (c, 3) ] ] ) ]
  assertBool "1" $ reconcileCondElts (S.fromList [ce,cf])
    == Just ( M.fromList
              [ (1, S.singleton $ M.fromList [ (a, 2), (b, 2) ] )
              , (2, S.singleton $ M.fromList [ (a,1), (b,1), (c,3) ] ) ] )

testReconcileCondEltsAtElt = TestCase $ do
  let (a,b,c,x) = (Var "a",Var "b",Var "c",Var "x")
      ce, cf :: CondElts
      ce = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 1) ]
                                       , M.fromList [ (a, 2), (b, 2) ] ] )
                      , (2, S.fromList [ M.fromList [ (a, 1), (b, 1) ] ] ) ]
      cf = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 2) ]
                                       , M.fromList [ (a, 2), (b, 2) ] ] )
                      , (2, S.fromList [ M.fromList [ (a, 1), (c, 3) ] ] ) ]
  assertBool "1" $ reconcileCondEltsAtElt 1 (S.fromList [ce,cf])
    == Just ( M.fromList
              [ (1, S.singleton $ M.fromList [ (a, 2), (b, 2) ] ) ] )
  assertBool "1" $ reconcileCondEltsAtElt 2 (S.fromList [ce,cf])
    == Just ( M.fromList
              [ (2, S.singleton
                  $ M.fromList [ (a,1), (b,1), (c,3) ] ) ] )
