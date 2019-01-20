{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation
-- Copyright   :  (c) DD.  2012
--                (c) LFL. 2009
-- License     :  BSD-style
-- Maintainer  :  Drew Day<drewday@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Relations are modeled as assciations between two elements.
--
-- Relations offer efficient search for any of the two elements.
--
-- Unlike "Data.Map", an element ca be associated more than once.
--
-- The two purposes of this structure are:
--
-- 1. Associating elements
--
-- 2. Provide efficient searches for either of the two elements.
--
-- Since neither 'map' nor 'fold' are implemented, you /must/ convert
-- the structure to a list to process sequentially.
--
--
module Data.Relation (
   Relation (..)
   -- ** Questions
 , size         --  # Tuples in the relation?
 , null         --  Is empty?

   -- ** Construction
 , empty        --  Construct an empty relation.
 , fromList     --  Relation <- []
 , singleton    --  Construct a relation with a single element.

   -- ** Operations
 , union        --  Union of two relations.
 , unions       --  Union on a list of relations.
 , intersection --  Intersection of two relations.
 , insert       --  Insert a tuple to the relation.
 , delete       --  Delete a tuple from the relation.
   -- The Set of values associated with a value in the domain.
 , lookupDom
   -- The Set of values associated with a value in the range.
 , lookupRan
 , memberDom    --  Is the element in the domain?
 , memberRan    --  Is the element in the range?
 , member       --  Is the tuple   in the relation?
 , notMember

   -- ** Conversion
 , toList --  Construct a list from a relation
          --  Extract the elements of the range to a Set.
 , dom    --  Extract the elements of the domain to a Set.
 , ran

  -- ** Invertible Relations
 , c

   -- ** Utilities
 , compactSet --  Compact a Set of Maybe's.

 -- $selectops
 , (|$>) -- Restrict the range according to a subset. PICA.
 , (<$|) -- Restrict the domain according to a subset. PICA.
 , (<|)  -- Domain restriction. Z.
 , (|>)  -- Range restriction. z.

 , mapRan,  mapDom,  filter
 , mapRan', mapDom', filter'
)

where

import           Prelude           hiding (null,filter)
import           Control.Arrow     (first, second)
import           Control.Monad     (MonadPlus, guard)
import           Data.Functor      (Functor((<$)))
import qualified Data.List    as L
import qualified Data.Map     as M
import qualified Data.Set     as S
import           Data.Maybe        (isJust, fromJust, fromMaybe)

-- |
-- This implementation avoids using @"S.Set (a,b)"@ because
-- it it is necessary to search for an item without knowing both @D@ and @R@.
--
-- In "S.Set", you must know both values to search.
--
-- Thus, we have are two maps to updated together.
--
-- 1. Always be careful with the associated set of the key.
--
-- 2. If you union two relations, apply union to the set of values.
--
-- 3. If you subtract, take care when handling the set of values.
--
-- As a multi-map, each key is asscoated with a Set of values v.
--
-- We do not allow the associations with the 'empty' Set.
--

data Relation a b  = Relation { domain ::  M.Map a (S.Set b)
                              , range  ::  M.Map b (S.Set a)
                              }

    deriving (Show, Eq, Ord)




-- * Functions about relations


-- The size is calculated using the domain.
-- |  @size r@ returns the number of tuples in the relation.

size    ::  Relation a b -> Int
size r  =   M.foldr ((+) . S.size) 0 (domain r)



-- | Construct a relation with no elements.

empty   ::  Relation a b
empty   =   Relation M.empty M.empty



-- |
-- The list must be formatted like: [(k1, v1), (k2, v2),..,(kn, vn)].

fromList    ::  (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList xs =
    Relation
        { domain =  M.fromListWith S.union $ snd2Set    xs
        , range   =  M.fromListWith S.union $ flipAndSet xs
        }
    where
       snd2Set    = map ( \(x,y) -> (x, S.singleton y) )
       flipAndSet = map ( \(x,y) -> (y, S.singleton x) )


-- |
-- Builds a List from a Relation.
toList   ::  Relation a b -> [(a,b)]
toList r =   concatMap
               ( \(x,y) -> zip (repeat x) (S.toList y) )
               ( M.toList . domain $ r)



-- |
-- Builds a 'Relation' consiting of an association between: @x@ and @y@.

singleton      ::  a -> b -> Relation a b
singleton x y  =   Relation
                     { domain = M.singleton x (S.singleton y)
                     , range   = M.singleton y (S.singleton x)
                     }



-- | The 'Relation' that results from the union of two relations: @r@ and @s@.

union ::  (Ord a, Ord b)
      =>  Relation a b -> Relation a b -> Relation a b

union r s       =
    Relation
      { domain =  M.unionWith S.union (domain r) (domain s)
      , range   =  M.unionWith S.union (range   r) (range   s)
      }


---------------------------------------------------------------
-- |
-- This fragment provided by:
--
-- @
-- \  Module      :  Data.Map
-- \  Copyright   :  (c) Daan Leijen 2002
-- \                 (c) Andriy Palamarchuk 2008
-- \  License     :  BSD-style
-- \  Maintainer  :  libraries\@haskell.org
-- \  Stability   :  provisional
-- \  Portability :  portable
-- @
--
--
foldlStrict         ::  (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs  =   case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)
---------------------------------------------------------------


-- | Union a list of relations using the 'empty' relation.

unions       ::  (Ord a, Ord b) => [Relation a b] -> Relation a b

unions       =   foldlStrict union empty



-- | Intersection of two relations: @a@ and @b@ are related by @intersection r
-- s@ exactly when @a@ and @b@ are related by @r@ and @s@.

intersection ::  (Ord a, Ord b)
             =>  Relation a b -> Relation a b -> Relation a b

intersection r s = Relation
  { domain = doubleIntersect (domain r) (domain s)
  , range  = doubleIntersect (range  r) (range  s)
  }


ensure :: MonadPlus m => (a -> Bool) -> a -> m a
ensure p x = x <$ guard (p x)

-- This function is like M.intersectionWith S.intersection except that it
-- also removes keys that would then be associated with empty sets.
doubleIntersect :: (Ord k, Ord v)
                => M.Map k (S.Set v)
                -> M.Map k (S.Set v)
                -> M.Map k (S.Set v)
doubleIntersect = M.mergeWithKey
  (\_ l r -> ensure (not . S.null) (S.intersection l r))
  (const M.empty)
  (const M.empty)


-- | Insert a relation @ x @ and @ y @ in the relation @ r @

insert       ::  (Ord a, Ord b)
             =>  a -> b -> Relation a b -> Relation a b

insert x y r =  -- r { domain = domain', range = range' }
                Relation domain' range'
  where
   domain'  =  M.insertWith S.union x (S.singleton y) (domain r)
   range'    =  M.insertWith S.union y (S.singleton x) (range   r)


-- $deletenotes
--
-- The deletion is not difficult but is delicate:
--
-- @
--   r = { domain {  (k1, {v1a, v3})
--                 ,  (k2, {v2a})
--                 ,  (k3, {v3b, v3})
--                 }
--       , range   {  (v1a, {k1}
--                 ,  (v2a, {k2{
--                 ,  (v3 , {k1, k3}
--                 ,  (v3b, {k3}
--                 }
--      }
-- @
--
--   To delete (k,v) in the relation do:
--    1. Working with the domain:
--       1a. Delete v from the Set VS associated with k.
--       1b. If VS is empty, delete k in the domain.
--    2. Working in the range:
--       2a. Delete k from the Set VS associated with v.
--       2b. If VS is empty, delete v in the range.
--
--

-- |  Delete an association in the relation.
delete       ::  (Ord a, Ord b)
             =>  a -> b -> Relation a b -> Relation a b

delete x y r  =  r { domain = domain', range = range' }
   where
   domain'   =  M.update (erase y) x (domain r)
   range'     =  M.update (erase x) y (range   r)
   erase e s =  if  S.singleton e == s
                     then  Nothing
                     else  Just $ S.delete e s

-- | The Set of values associated with a value in the domain.

lookupDom     ::  Ord a =>  a -> Relation a b -> S.Set b
lookupDom x r =   fromMaybe S.empty
              $   M.lookup  x  (domain r)



-- | The Set of values associated with a value in the range.

lookupRan     ::  Ord b =>  b -> Relation a b -> S.Set a
lookupRan y r =   fromMaybe S.empty
              $   M.lookup  y  (range   r)



-- | True if the element @ x @ exists in the domain of @ r @.

memberDom     ::  Ord a =>  a -> Relation a b -> Bool
memberDom x r =   not . S.null $ lookupDom x r



-- | True if the element exists in the range.

memberRan     ::  Ord b =>  b -> Relation a b -> Bool
memberRan y r =   not . S.null $ lookupRan y r



-- |
-- True if the relation @r@ is the 'empty' relation.
null    ::  Relation a b -> Bool
null r  =   M.null $ domain r
-- Before 2010/11/09 null::Ord b =>  Relation a b -> Bool



-- | True if the relation contains the association @x@ and @y@

member       ::  (Ord a, Ord b) =>  a -> b -> Relation a b -> Bool
member x y r =   S.member y (lookupDom x r)



-- | True if the relation /does not/ contain the association @x@ and @y@

notMember       ::  (Ord a, Ord b) =>  a -> b -> Relation a b -> Bool
notMember x y r =   not $ member x y r



-- | Returns the domain in the relation, as a Set, in its entirety.

dom            ::  Relation a b -> S.Set a
dom r          =   M.keysSet (domain r)



-- | Returns the range of the relation, as a Set, in its entirety.

ran            ::  Relation a b -> S.Set b
ran r          =   M.keysSet (range   r)


-- | Returns the converse of the relation.
c :: Relation a b -> Relation b a

c r = Relation {
                    domain = range'
                    ,range  = domain'
               }
     where
           range' = range r
           domain' = domain r

-- |
-- A compact set of sets the values of which can be @Just (Set x)@ or @Nothing@.
--
-- The cases of 'Nothing' are purged.
--
-- It is similar to 'concat'.
compactSet ::  Ord a => S.Set (S.Set a) -> S.Set a

compactSet =   S.foldr S.union S.empty



-- $selectops
--
-- Primitive implementation for the /right selection/ and /left selection/ operators.
--
-- PICA provides both operators:
--        '|>'  and  '<|'
-- and    '|$>' and '<$|'
--
-- in this library, for working with Relations and OIS (Ordered, Inductive Sets?).
--
-- PICA exposes the operators defined here, so as not to interfere with the abstraction
-- of the Relation type and because having access to Relation hidden components is a more
-- efficient implementation of the operation of restriction.
--
-- @
--     (a <$| b) r
--
--       denotes: for every element     @b@ from the Set      @B@,
--                select an element @a@     from the Set @A@     ,
--                              if  @a@
--                   is related to      @b@
--                   in @r@
-- @
--
-- @
--     (a |$> b) r
--
--       denotes: for every element @a@      from the Set @A@    ,
--                select an element     @b@  from the Set     @B@,
--                              if  @a@
--                   is related to      @b@
--                   in @r@
-- @
--
-- With regard to domain restriction and range restriction operators
-- of the language, those are described differently and return the domain or the range.

-- |
-- @(Case b <| r a)@
--
(<$|)          ::  (Ord a, Ord b)
               =>  S.Set a -> S.Set b -> Relation a b -> S.Set a

(as <$| bs) r  =   as `S.intersection` generarAS bs

    where  generarAS = compactSet . S.map (`lookupRan` r)

    -- The subsets of the domain (a) associated with each @b@
    -- such that @b@ in @B@ and (b) are in the range of the relation.
    -- The expression 'S.map' returns a set of @Either (S.Set a)@.


-- |
-- @( Case a |> r b )@
(|$>)          ::  (Ord a, Ord b)
               =>  S.Set a -> S.Set b -> Relation a b -> S.Set b

(as |$> bs) r  =   bs `S.intersection`  generarBS as

    where  generarBS = compactSet . S.map (`lookupDom` r)



-- | Domain restriction for a relation. Modeled on z.

(<|) :: (Ord a, Ord b) => S.Set a -> Relation a b  -> Relation a b

s <| r  =  fromList $ concatMap
               ( \(x,y) -> zip (repeat x) (S.toList y) )
               ( M.toList domain' )
    where
    domain'  =  M.unions . map filtrar . S.toList $ s
    filtrar x =  M.filterWithKey (\k _ -> k == x) dr
    dr        =  domain r  -- just to memoize the value


-- | Range restriction for a relation. Modeled on z.

(|>) :: (Ord a, Ord b) => Relation a b -> S.Set b -> Relation a b

r |> t =  fromList $ concatMap
               ( \(x,y) -> zip (S.toList y) (repeat x) )
               ( M.toList range' )
    where
    range'    =  M.unions . map filtrar . S.toList $ t
    filtrar x =  M.filterWithKey (\k _ -> k == x) rr
    rr        =  range r   -- just to memoize the value


-- Note:
--
--    As you have seen this implementation is expensive in terms
--    of storage. Information is registered twice.
--    For the operators |> and <| we follow a pattern used in
--    the @fromList@ constructor and @toList@ flattener:
--    It is enough to know one half of the Relation (the domain or
--    the range) to create to other half.
--
--


   -- ** Maps and filters

mapDom :: (Ord a', Ord b) => (a -> a') -> Relation a b -> Relation a' b
mapDom f = fromList . L.map (first f) . toList

mapRan :: (Ord a, Ord b') => (b -> b') -> Relation a b -> Relation a b'
mapRan f = fromList . L.map (second f) . toList

filter :: (Ord a, Ord b) => (a -> b -> Bool) -> Relation a b -> Relation a b
filter f = fromList . L.filter (uncurry f) . toList

-- | = These versions are probably rarely a good idea. They require running
-- the function argument at least twice as many times. But if the `Set`s
-- involved are huge, and the function argument simple, it's conceivable
-- that you would prefer rerunning the function argument to deconstructing
-- and reconstructing the `Set`s.

mapDom' :: Ord a' => (a -> a') -> Relation a b -> Relation a' b
mapDom' f rel = Relation d r where
  d = M.mapKeys f     $ domain rel
  r = M.map (S.map f) $ range rel

mapRan' :: Ord b' => (b -> b') -> Relation a b -> Relation a b'
mapRan' f rel = Relation d r where
  d = M.map (S.map f) $ domain rel
  r = M.mapKeys f     $ range rel

filter' :: forall a b. (Ord a, Ord b)
        => (a -> b -> Bool) -> Relation a b -> Relation a b
filter' f rel = Relation d r where
  d = M.filter (not . S.null) $ M.mapWithKey f' $ domain rel where
    f' :: a -> S.Set b -> S.Set b
    f' a = S.filter $ f a
  r = M.filter (not . S.null) $ M.mapWithKey f' $ range rel where
    f' :: b -> S.Set a -> S.Set a
    f' b = S.filter $ flip f b
