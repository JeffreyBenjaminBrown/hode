# Nested existentialy and universally quantifiable search over arbitrary spaces

## Introduction: An example search

Suppose your space is a graph, equipped with functions "parents" and "children", both of which have the type `Graph e -> e -> Set e`. (See Graph.hs for one such implementation.) Suppose further that you'd like to define the following program:

Find every child of 2.
  Call That collection "a".
Find every node "n" satisfying the following critieria:
  For some element "a1" of the set "a" found in the previous step,
    n should be a child of "a1"
    n should not be equal to "a1", nor to the node 2.
  Call that collection "b".

Here's how you would write that:

```
[ ( "a", QFind $ findParents $ Left 2)
, ( "b", ( ForSome "a1" (Source "a") $
           QAnd [ QFind $ mkFind "children" children $ Right "a1"
                , QTest $ mkTest (/=) $ Right "a1"
                , QTest $ mkTest (/=) $ Left 2 ] ) ) ]
```

See Test/TProgram.hs for that very Program in action. (The String argument to `mkFind` is only there to make errors more intelligible.)


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

A Find is used to find stuff in the space. You can make something of type Find by running mkFind. mkFind expects a function of type `(sp -> e -> Set e)`. That is, given a space sp and an element e of it, this function can produce a bunch of other elements of the space.

A Test is used to filter things that have been Found. You can make a Test by running mkTest, which expects a function of type `(e -> e -> Bool)`.

Everywhere you use a quantifier (ForAll or ForSome), a variable is bound to every possible value of that quantifier's Source. A VarTest is used to reduce the combinations of variables tried. You can make VarTests with mkVarTest. Here's an example:

```
    [ ("a", QFind $ findChildren $ Left 0)
    , ("b", ( ForSome "a1" (Source "a")
              ( ForSome "a2" (Source "a")
                (QAnd [ QVarTest $ mkVarTest (<) (Right "a1") (Right "a2")
                      , QFind $ findChildren $ Right "a1"
                      , QFind $ findChildren $ Right "a2" ] ) ) ) ) ]
```

would find all nodes "n" such that there exist two members "a1" and "a2" of the set "a" for which "a1" < "a2" and "n" is a child of both "a1" and "a2". (See Test/TProgram.hs for this very Program in action.)
