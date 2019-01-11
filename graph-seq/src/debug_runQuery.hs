runFindable d p q s
= runQAnd d p qs s

  (searches,tests) = partition findable qs
  found = map (flip (runFindable d p) s) searches :: [CondElts]

head found = runFindable d p (ForSome aOf_b $ fc aOf_b) s :: CondElts

runFindable d p (ForSome v q) s
  v = aOf_b
  q = fc aOf_b -- this shadows the earlier q
  vp = varPossibilities p s v
     = Map [ (2, Set mempty), (3, Set mempty) ]

= fromList [(Var a of [],fromList [(1,fromList [fromList []])
                                   ,(2,fromList [fromList []])
                                   ,(3,fromList [fromList []])])
           ,(Var a of [Var b of []],fromList [(2,fromList [fromList []])
              -- TODO : Shouldn't these Substs include b?
                                             ,(3,fromList [fromList []])])
           ,(Var b of [],fromList [(11,fromList [fromList [(Var a of [],1)]])
                                  ,(23,fromList [fromList [(Var a of [],2)]
                                                ,fromList [(Var a of [],3)]])])]
