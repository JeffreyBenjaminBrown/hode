-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HTypes where


type Level = Int
type Joint = String

data PRel -- ^ a parser for `Rel`s
  = Absent -- ^ The leftmost and rightmost members of an `Open` or `Closed`
    -- might be absent. Interior ones should not be.
  | Leaf String
  | Closed     [PRel] [Joint] -- ^ First list: members. Second: joints.
    -- Only the first and last members can be Absent. |joints| = |members| - 1
  | Open Level [PRel] [Joint] -- ^ Like `Closed`, but more things
    -- might be inserted into it.
  deriving (Eq, Show)

hash :: Level -> Joint -> PRel -> PRel -> Either String PRel
hash l j
  a@(isOpen -> False)
  b@(isOpen -> False)
  = Right $ startOpen l j a b
hash l j
  a@(Open l' mbrs js)
  b@(isOpen -> False)
  | l < l'  = Left $ "hash, with args l=" ++ show l ++ ", j=" ++ show j
              ++ ", a=" ++ show a ++ ", b=" ++ show b
              ++ ": higher level should not have been evaluated first."
  | l == l' = mergeIntoLeft j a b
  | l > l'  = Right $ startOpen l j a b
hash l j
  a@(isOpen -> False)
  b@(Open l' mbrs js)
  | l < l'  = Left $ "hash, with args l=" ++ show l ++ ", j=" ++ show j
              ++ ", a=" ++ show a ++ ", b=" ++ show b
              ++ ": higher level should not have been evaluated first."
  | l == l' = mergeIntoRight j a b
  | l > l'  = Right $ startOpen l j a b
hash l j
  a@(Open la _ _)
  b@(Open lb _ _)
  | l < (min la lb)  = Left
    $ "hash, with args l=" ++ show l ++ ", j=" ++ show j
    ++ ", a=" ++ show a ++ ", b=" ++ show b
    ++ ": higher level should not have been evaluated first."
  | l == la          = mergeIntoLeft j a b
  | l == lb          = mergeIntoRight j a b
  | l > (max la lb)  = Right $ startOpen l j a b

mergeIntoLeft :: Joint -> PRel -> PRel -> Either String PRel
mergeIntoLeft j (Open l mbrs joints) pr =
  Right $ Open l (pr:mbrs) (j:joints)
mergeIntoLeft j (Closed mbrs joints) pr =
  Right $ Closed (pr:mbrs) (j:joints)
mergeIntoLeft _ pr _ = Left $ "mergeIntoLeft: PRel " ++ show pr
  ++ " cannot receive \"more\" members."

mergeIntoRight :: Joint -> PRel -> PRel -> Either String PRel
mergeIntoRight j pr (Open l mbrs joints) =
  Right $ Open l (pr:mbrs) (j:joints)
mergeIntoRight j pr (Closed mbrs joints) =
  Right $ Closed (pr:mbrs) (j:joints)
mergeIntoRight _ _ pr = Left $ "mergeIntoRight: PRel " ++ show pr
  ++ "cannot receive \"more\" members."

startOpen :: Level -> Joint -> PRel -> PRel -> PRel
startOpen l j a b = Open l [a,b] [j]

isOpen :: PRel -> Bool
isOpen (Open _ _ _) = True
isOpen _ = False

--pRelMembers :: PRel -> Either String [PRel]
--pRelMembers (Closed mbrs _) = Right mbrs
--pRelMembers (Open _ mbrs _) = Right mbrs
--pRelMembers pr = Left
--  $ "pRelMembers: PRel " ++ show pr ++ " has no members."
 
--higher :: PRel -> PRel -> Bool
--higher (Leaf _    ) (Leaf _    ) = True
--higher (Leaf _    ) (Closed _ _) = False
--higher (Leaf _    ) (Open _ _ _) = False
--higher (Closed _ _) (Closed _ _) = True
--higher (Closed _ _) (Open _ _ _) = False
--higher (Open l _ _) (Open m _ _) = l <= m
--higher a b = not $ higher b a
