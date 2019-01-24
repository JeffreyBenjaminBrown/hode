{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Query.Leaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Query.Inspect
import Subst
import Types
import Util


-- | == Atomic queries. Bigger queries are made of junctions and quantifiers over these.

unEitherEltVar :: Subst e -> Either e Var -> (Maybe e, Maybe Var)
unEitherEltVar _ (Left e) = (Just e, Nothing)
unEitherEltVar s (Right v) = (M.lookup v s, Just v)


-- | = `Test`s

mkTest :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Test e sp
mkTest compare eev = Test go deps where
  go :: sp -> Subst e -> e -> Bool
  go _ s = let (me, mv) = unEitherEltVar s eev :: (Maybe e, Maybe Var)
               err = error $ keyErr "mkTest" (fromJust mv) s
           in maybe err compare me
  deps :: Set Var
  deps = S.fromList $ catMaybes $ map (either (const Nothing) Just) [eev]


-- | = `VarTest`s

mkVarTest :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Either e Var -> VarTest e sp
mkVarTest compare eev eev' = VarTest go deps where
  go :: sp -> Subst e -> Bool
  go _ s = let (me , mv ) = unEitherEltVar s eev  :: (Maybe e, Maybe Var)
               (me', mv') = unEitherEltVar s eev' :: (Maybe e, Maybe Var)
               err v = error $ keyErr "mkVarTest" (fromJust v) s
    in case me of Nothing -> err mv
                  Just e -> case me' of Nothing -> err mv'
                                        Just e' -> compare e e'
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']


-- | = `Find`s

-- | Run a search that does not depend on the `Subst`.
mkFind :: forall e sp. (Show e) =>
  String -> (sp ->      Set e) ->                 Find e sp
-- | Run a search starting from some element of the space.
mkFindFrom :: forall e sp. (Show e) =>
  String -> (sp -> e -> Set e) -> Either e Var -> Find e sp

mkFind findName finder = Find go S.empty where
  go :: sp -> Subst e -> Set e
  go g _ = finder g
mkFindFrom findName finder eev = Find go deps where
  go :: sp -> Subst e -> Set e
  go g s = maybe err (finder g) me where
      (me, mv) = unEitherEltVar s eev :: (Maybe e, Maybe Var)
      err = error $ keyErr findName (fromJust mv) s
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev]
