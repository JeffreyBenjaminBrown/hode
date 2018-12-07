{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

module Index.ImgLookup where

import Maybe
import qualified Map as M

import Rslt
import Util (hasANothing)


exprVariety :: Expr -> (Expr', Arity)
exprVariety   (Word      _)   = (Word'      , 0)
exprVariety e@(Tplt  _)   = (Tplt'  , arity e)
exprVariety e@(Rel       _ _) = (Rel'       , arity e)
exprVariety e@(Par _ _) = (Par' , arity e)

imgDb :: Files -> FM Expr Addr
imgDb = listToFM (<) . catMaybes . map f . fmToList where
  f (addr, expr) = case exprImgKey expr of
    Nothing -> Nothing
    _       -> Just (expr, addr)

  exprImgKey :: Expr -> Maybe Expr
  exprImgKey e = case e of Par _ _ -> Nothing
                           _             -> Just e

imgLookup :: Files -> ImgOfExpr -> Maybe Addr
imgLookup files img = let idb = imgDb files in case img of

  ImgOfExpr    e -> lookupFM idb e
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ lookupFM files a

  ImgOfTplt is ->
    let mas = map (imgLookup files) is
    in case hasANothing mas of
         True -> Nothing
         False -> lookupFM idb $ Tplt $ catMaybes mas

  ImgOfRel is i ->
    let mas = map (imgLookup files) is
        ma = imgLookup files i
    in case hasANothing mas || isNothing ma of
         True -> Nothing
         False -> lookupFM idb $ Rel (catMaybes mas) $ fromJust ma
