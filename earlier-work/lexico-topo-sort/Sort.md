For more details on the algorithm, but possibly redundant or stale, see Sort.org. Sort.hs, which defines a couple types, might help too.


# The setup

We're given templates T1 > T2 > ... Tn, and a set of elements E, in a graph.
We would like to lexicographically sort E by the Ti,
such that later templates are only used to break ties from earlier ones.


# I'm not sure lexicographic toposort is worth it

Given its complexity,
I should probably wait until I at least wish I had it.


# My multi-edge-label sort algorithm is slower than Kahn's single-label one

In Kahn's algorithm, each time you consume a top, it points you at where to go next. This lets you traverse the graph quickly. In my algorithm that kind of information is lost.


# The algorithm in a nutshell

Let S0 be the subgraph induced by the Ti and E.

Extend a "survivors" list" L.
  When starting the algorithm, L will be the singleton [S0];
    however other "survivors" lists will need extending, too.
  Find everything maximal w/r/t T1. Call it S1 (for "survived T1"),
    and prepend it to L. Note that S1 might be empty.
  From the first nonempty set in L, find eveyrthing maximal w/r/t T2.
    Call that S2, and prepend it to L.
  Repeat that until either
    (a) creating some singleton Sk.
    (b) creating Sn, for the same n as in Tn.
  Now we have a "fully extended" survivors list.

Consume the top of a fully extended survivors list.

Extend. Consume. Extend. Consume ... voila.
