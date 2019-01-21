Haskell: when handling collections of Eithers, reduce clutter|boilerplate 

I've got Either threaded throughout [some code](https://github.com/JeffreyBenjaminBrown/rslt-haskell/blob/d1b6778a4d8ba7296fcc07ec1e26d60c5c7c1e23/src/RelExperim.hs), and it's cluttering to the point of unreadability functions that would otherwise be simple. The problem is that I often have to run some Either-producing function on a list. If any Lefts are produced, I need to collect all of their messages, prepend an introductory message, and return that. Otherwise the calculation can proceed.

Here's an example. It is almost entirely the handling of Eithers.

    input_matched_varPossibilities' p s t = let
      (pls, ins) = -- definition not important
      sources_either, lefts :: [Either String (CondElts e)]
      sources_either = map (fetch . tSource) pls where
        fetch :: Var -> Either String (CondElts e)
        fetch v = maybe (Left $ keyErr "fetch" v p) Right
          $ M.lookup v p
      lefts = filter isLeft sources_either
      in case null lefts of
        False -> Left $ "input_matched_varPossibilities': error in callee:\n"
                 ++ concat (map (fromLeft "") lefts)
        True -> let
      
          sources :: [CondElts e]
          sources = map (fromRight $ error "impossible") sources_either

          sources_ins_matched_either, lefts :: [Either String (CondElts e)]
          sources_ins_matched_either = -- definition not important
          lefts = filter isLeft sources_ins_matched_either
          in case null lefts of
            False -> Left $ "input_matched_varPossibilities': error in callee:\n"
                     ++ concat (map (fromLeft "") lefts)
    
            True -> let
              sources_ins_matched =
                map (fromRight $ error "impossible") sources_ins_matched_either
              in Right $ -- some function of sources_ins_matched
    
Is there some way to do this with less boilerplate?
