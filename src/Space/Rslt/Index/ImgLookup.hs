module Space.Rslt.Index.ImgLookup where

import           Data.Maybe (catMaybes, isNothing, fromJust)
import qualified Data.Map as M

import Space.Rslt


exprVariety :: Expr -> (Expr', Arity)
exprVariety   (Word  _) = (Word' , 0)
exprVariety e@(Tplt  _) = (Tplt' , arity e)
exprVariety e@(Rel _ _) = (Rel'  , arity e)
exprVariety e@(Par _ _) = (Par'  , arity e)

imgDb :: Files -> M.Map Expr Addr
imgDb = M.fromList . catMaybes . map f . M.toList where
  f (addr, expr) = case exprImgKey expr of
    Nothing -> Nothing
    _       -> Just (expr, addr)

  exprImgKey :: Expr -> Maybe Expr
  exprImgKey e = case e of Par _ _ -> Nothing
                           _       -> Just e

imgLookup :: Files -> ImgOfExpr -> Maybe Addr
imgLookup files img = let idb = imgDb files in case img of

  ImgOfExpr e -> M.lookup e idb
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a files

  ImgOfTplt is ->
    let mas = map (imgLookup files) is
    in case or $ map isNothing $ mas of
         True  -> Nothing
         False -> M.lookup (Tplt $ catMaybes mas) idb

  ImgOfRel is i ->
    let mas = map (imgLookup files) is
        ma = imgLookup files i
    in case or (map isNothing mas) || isNothing ma of
         True  -> Nothing
         False -> M.lookup (Rel (catMaybes mas) $ fromJust ma) idb
