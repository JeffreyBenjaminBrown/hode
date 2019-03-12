-- | This code could be a lot shorter if I understood and/or used:
-- (1) prisms? traversals?) something to let me lens into a Vector
-- to modify it, where the return type is `Either String (Vector a)`
-- rather than `Vector a`.
-- (2) Zippers instead of Vectors. (This would obviate the first task.)
-- (2a) Mutable Vectors instead of immutable ones.


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.ViewTree where

import qualified Data.Vector as V
import           Lens.Micro

import UI.ITypes2
import Util.Misc


-- TODO : The next two functions should be (prismatic?) one-liners.
-- TODO : The `Vector View` field really ought (so far) to be a zipper.
get_viewAt :: [Int] -> View -> Either String View
get_viewAt [] v = Right v
get_viewAt (p:path) v = do
  let (subvs :: V.Vector View) = v ^. viewSubviews
  _ <- let errMsg = "get_viewAt: index " ++ show p ++ " out of bounds."
       in if inBounds subvs p then Right () else Left errMsg
  let subv = (V.!) subvs p
  get_viewAt path subv


mod_viewAt :: [Int] -> (View -> View) -> View -> Either String View
mod_viewAt []       f v = Right $ f v
mod_viewAt (p:path) f v = do
  let (subvs :: V.Vector View) = v ^. viewSubviews
  _ <- let errMsg = "get_viewAt: index " ++ show p ++ " out of bounds."
       in if inBounds subvs p then Right () else Left errMsg
  let subv = (V.!) subvs p
  subv' <- mod_viewAt path f subv
  let Just subvs' = modifyAt p (const subv') subvs
      -- it's not `Nothing` because I already checked `inBounds`.
  Right $ v & viewSubviews .~ subvs'


moveFocus :: Direction -> St2 -> Either String St2
moveFocus DirLeft st@( _st2_pathToFocus -> [] ) = Right st
moveFocus DirLeft st = Right $ st & st2_pathToFocus %~ tail

moveFocus DirRight st = do
  (v :: View) <- prefixLeft "moveFocus"
    $ get_viewAt (st ^. st2_pathToFocus)
    (st ^. st2_view)
  case null $ v ^. viewSubviews of
    True -> Right st
    False -> let nextFocusLink = v ^. viewFocus
      in Right $ st & st2_pathToFocus %~ (++ [nextFocusLink])

moveFocus DirUp st = do
  let path = st ^. st2_pathToFocus
      pathToParent = take (length path - 1) path
      topView = st ^. st2_view
  (parent :: View) <- prefixLeft "moveFocus"
    $ get_viewAt pathToParent topView
  let parFoc = parent ^. viewFocus
      parFoc' = if inBounds (parent ^. viewSubviews) parFoc
        then max 0 $ parFoc - 1
        else 0
      path' = path ++ [parFoc']
  topView' <- mod_viewAt pathToParent
              (viewFocus .~ parFoc') topView
  Right $ st & st2_pathToFocus .~ path'
    & st2_view .~ topView'

moveFocus DirDown st = do
  let path = st ^. st2_pathToFocus
      pathToParent = take (length path - 1) path
      topView = st ^. st2_view
  (parent :: View) <- prefixLeft "moveFocus"
    $ get_viewAt pathToParent topView
  let parFoc = parent ^. viewFocus
      parFoc' = if inBounds (parent ^. viewSubviews) parFoc
        then min (parFoc + 1) $ V.length (parent ^. viewSubviews) - 1
        else 0
      path' = path ++ [parFoc']
  topView' <- mod_viewAt pathToParent
              (viewFocus .~ parFoc') topView
  Right $ st & st2_pathToFocus .~ path'
    & st2_view .~ topView'
