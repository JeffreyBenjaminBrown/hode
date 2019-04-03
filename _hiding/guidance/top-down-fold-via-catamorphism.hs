-- Thanks to @rampion on Reddit Haskell:
-- https://www.reddit.com/r/haskellquestions/comments/azn4pb/topdown_fold_stated_in_cateogry_theory_as_a/

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Functor.Foldable
import Data.Functor.Foldable.TH


data View = View { content :: String
                 , subViews :: [View] }
          deriving (Eq, Show)
makeBaseFunctor ''View


xWrite :: View -> [String]
xWrite = flip xWrite' 0 where
  xWrite' :: View -> Int -> [String]
  xWrite' = cata go where
    go :: Base View (Int -> [String]) -> Int -> [String]
    go (ViewF s fs) i = [indent i s] ++ concatMap ($ (i+1)) fs where
      indent :: Int -> String -> String
      indent i0 s0 = replicate (4*i0) ' ' ++ s0

demo :: IO ()
demo = let v = View in
  mapM_ putStrLn $ xWrite $
  v "a" [v "b" [v "c" []
               , v "d" []]
        , v "e" []]
