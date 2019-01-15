# Nested existentialy and universally quantifiable search over arbitrary spaces

## An example search

Suppose you wanted to define the following program:
Find every child of 2.
  Call That collection "a".
Find every node "n" satisfying the following critieria:
  For some element "a1" of the set "a" found in the previous step,
    n should be a child of "a1"
    n should not be equal to "a1", nor to the node 2.
  Call that collection "b".

Here's how you would write that:    
    
    [ ( "a", QFind $ findParents $ Left 2)
    , ( "b", ( ForSome "a1" (Source "a") $
               QAnd [ QFind $ findChildren $ Right "a1"
                    , QTest $ isNot_1 $ Right "a1"
                    , QTest $ isNot_1 $ Left 2 ] ) ) ]


## The full vocabulary

The Query type looks like this:

```
data Query e sp = QFind    (Find    e sp)
                | QTest    (Test    e sp)
                | QVarTest (VarTest e sp)
                | QAnd               [Query e sp] -- ^ order not important
                | QOr                [Query e sp] -- ^ order not important
                | ForAll  Var Source (Query e sp)
                | ForSome Var Source (Query e sp)
```

A Query is generic over the fields `e` and `sp`: The first describes what a member of the space we are searching for is, and the second, what the space is. The example used throughout the test suite is Query Int (Graph Int) -- it searches for Ints within a Graph the nodes of which are Ints.

The QAnd and QOr constructors take a list of Queries and find their intersection or their union respectively. The ForAll and ForSome constructors take a variable name, the source from which to draw values of that variable, and a Query which can refer to the new variable.

The QFind, QTest and QVarTest constructors are defined in terms of the Find, Test and VarTest types. See the code for the full spec, but here they are in brief:

The important part of Find is a function of type `sp -> Subst e -> Set e`. `Subst e` means `substitution`, a term I'm borrowing from Prolog: it is an assignment of variable names (Strings) to values of type `e`. So a Find is capable of, given a space and an assignment of variable names to elements of that space, to return a set of elements of the space. An example would be "find all the children of the variable x", or "find all the children of 2". (Note that the latter makes sense even with an empty Subst; the former only makes sense if the Subst assigns a value to the string "x".)

A Test includes a function of type `sp -> Subst e -> e -> Bool`. Tests are used to filter the results of Finds.

A VarTest includes a function of type `sp -> Subst e -> Bool`. A VarTest is useful for narrowing the set of Substs under consideration. For instance, the Query


-- TODO: This doesn't work.


```
ForSOme "a1" (Source "a")
  (ForSOme "a2" (Source "a")
    (QAnd [ QVarTest $ isNot_2 (Right "a1") (Right "a2")
          , QFind $ findChildren $ Right "a1"
          , QFind $ findChildren $ Right "a2" ] ) )
```
  
would find all nodes "n" such that for two distinct members "a1" and "a2" of the set "a", "n" is a child of both "a1" and "a2".