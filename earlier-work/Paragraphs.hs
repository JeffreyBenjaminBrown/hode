-- | Rslt.Types

data Par a = Par [(String, a)] String
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)

data Expr =
  ...
  | ExprPar (Par Expr) -- ^ "Paragraph".
    -- The `String`s in a `Par` are like a single-use `Tplt`.
    -- A `Par` has Members, but (unlike a `Rel`) no `Tplt`.
    -- `Par`s are like `Tplt`s, in that |Members| + 1 = |`String`s|.
    -- `Par`s are like lists, in that the weird bit comes last.
    -- `Par` is the only kind of `RefExpr` not in the `Index`.

data RefExpr =
  ...
  | Par' (Par Addr)

data ExprCtr = PhraseCtr | RelCtr | TpltCtr | ParCtr


-- | Hash.Types
data PExpr = -- ^ intermediate type, on the way to parsing a `Rel`
    PExpr Expr
  | PMap PMap
  | PEval PExpr
  | PVar Var
  | PDiff PExpr PExpr
  | PAnd [PExpr]
  | POr [PExpr]
  | Any
  | It (Maybe PExpr)
  | PPar (Par PExpr)
  | PRel PRel
   deriving (Eq, Show)


-- | Rslt.Util
ifLefts_par :: String -> Par (Either String a) -> Either String (Par a)
ifLefts_par errMsg (Par pairs s) = let
  lefts = filter isLeft $ map snd pairs
  impossible = error "ifLefts: impossible."
  in case null lefts of
       True -> Right $ Par (map (_2 %~ fromRight impossible) pairs) s
       False -> Left $ errMsg ++ ": "
                ++ concat (map (fromLeft impossible) lefts)

-- | Rslt.Lookup.Convert

refExprToExpr r (Par' (Par sas s)) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [RefExpr]) <- ifLefts "refExprToExpr" $ map (addrToRefExpr r) as
  (eis :: [Expr])    <- ifLefts "refExprToExpr" $ map (refExprToExpr r) es
  Right $ ExprPar $ Par (zip ss eis) s
