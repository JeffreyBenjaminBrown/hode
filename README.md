# An EDSL for logical search over arbitrary spaces, with existential and universal quantification


## Introduction

Suppose your space is a graph, equipped with functions "parents" and "children", both of which have the type `Graph e -> e -> Set e`. (See Graph.hs for a tiny (50 lines) implementation of such a space.) Suppose further that you'd like to define the following program:

```
Find every child of 2.
  Call That collection "a".
Find every node "n" satisfying the following critieria:
  For some element "a1" of the set "a" found in the previous step,
    n should be a child of "a1"
    n should not be equal to "a1", nor to the node 2.
  Call that collection "b".
```

Here's how you would write that:

```
[ ( "a", QFind $ findParents $ Left 2)
, ( "b", ( ForSome "a1" (Source "a") $
           QAnd [ QFind $ mkFindFrom "children" children $ Right "a1"
                , QTest $ mkTest (/=) $ Right "a1"
                , QTest $ mkTest (/=) $ Left 2 ] ) ) ]
```

See Test/TProgram.hs for that very program in action. (The String argument to `mkFindFrom` is only there to make errors more intelligible.)


## The full language

A program is a list of `(String,Query)` pairs. The `Query` finds some stuff, which is then stored under the `String` label. Later `Query`s can refer to the results of earlier ones.

The `Query` type looks like this:

```
data Query e sp = QFind    (Find    e sp)
                | QTest    (Test    e sp)
                | QVTest (VarTest e sp)
                | QAnd               [Query e sp] -- ^ order not important
                | QOr                [Query e sp] -- ^ order not important
                | ForAll  Var Source (Query e sp)
                | ForSome Var Source (Query e sp)
```

`Query` is generic over the fields `e` and `sp`. `e` gives the type of members of the space we are searching, and `sp` the type of the space itself. The example used throughout the test suite is `Query Int (Graph Int)`; such queries search for `Int`s within a `Graph` the nodes of which are `Int`s.

The `QAnd` and `QOr` constructors take a list of Queries and find their intersection or their union respectively. The `ForAll` and `ForSome` constructors take a variable name, the source from which to draw values of that variable, and a `Query` which can refer to the new variable.

The `QFind`, `QTest` and `QVTest` constructors are defined in terms of the Find, Test and VarTest types. See the code for the full spec, but here they are in brief:

A `Find` is used to find stuff in the space. You can make something of type `Find` by running `mkFindFrom`. `mkFindFrom` expects a function of type `(sp -> e -> Set e)`. That is, given a space sp and an element e of it, this function can produce a bunch of other elements of the space.

A `Test` is used to filter things that have been found. You can make a `Test` by running `mkTest`, which expects a function of type `(e -> e -> Bool)`.

Everywhere you use a quantifier (`ForAll` or `ForSome`), a variable is bound to every possible value of that quantifier's `Source`. A `VarTest` is used to reduce the combinations of variables tried. You can make `VarTests` with `mkVarTest`. Here's an example:

```
    [ ("a", QFind $ findChildren $ Left 0)
    , ("b", ( ForSome "a1" (Source "a")
              ( ForSome "a2" (Source "a")
                (QAnd [ QVTest $ mkVarTest (<) (Right "a1") (Right "a2")
                      , QFind $ findChildren $ Right "a1"
                      , QFind $ findChildren $ Right "a2" ] ) ) ) ) ]
```

That program would first find all children of 0, and label that collection "a". Then it would find all nodes "n" such that there exist two members "a1" and "a2" of the collection "a" for which "a1" < "a2" and "n" is a child of both "a1" and "a2". (See Test/TProgram.hs for this very program in action.)

(`Find`, `Test` and `VarTest` are actually more general than that; all of them can refer to the set of currently-bound variables (the `Subst`), and they can take varying numbers of arguments. But the `mk*` family of functions is surely sufficient for most purposes.)


## Some idiosyncracies of the language

Most of these constraints are common sense, but alas not quite all of them.


### Natural constraints

Variables should not shadow each other.

A `Source` cannot be referred to before it has been defined. For instance, the following program is not valid:
```
    [ ( "a", QFind $ findParents $ Right "a" ...) ]
```

Every `QAnd` must include at least one findlike clause, and every `QOr` must contain nothing but findlike clauses. (See Inspect.hs for the precise definition of `findlike`.)

`QTest` and `QVTest`, since they are not findlike, can only be invoked as a clause in a `QAnd`. (Both are filters; it doesn't make sense to run them without first having found something to filter.)


### Not entirely natural

The variables introduced by the clauses of a `QAnd` must be distinct.

A variable should not bear the same name as one of the `Source`s. That is, while this is valid:
```
    [ ("a", ...)
    , ("b", ForSome "a1" (Source "a") ...) ]
```

this is not:
```
    [ ("a", ...)
    , ("b", ForSome "a" (Source "a") ...) ]
```
