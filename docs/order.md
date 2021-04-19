# Background: Order, transitive relationships, and cycles

## Not all relationships admit order

It can be reasonable to order data by certain relationships.
For instance, there might be a "greater than" relationship in your data,
which you might represent with the template `/_ #< /_`.
If you had some numbers on the screen,
you could order them from least to greatest, or vice-versa.

But if your graph had a `/_ has met /_`,
and you had Bill and Mary on the screen, both of whom have met each other,
who should come first? There's no good answer --
because `/_ has met /_` is not transitive.

## Transitive templates admit order

A transitive template `t` satisfies the property that if
`a #t b` and `b #t c`, then `a #t c`.

`/_ is greater than /_` relationships are transitive:
If `a` is greater than `b`, and `b` is greater than `c`,
then `a` is greater than `c`.

Transitive relationships cannot form cycles.
This prevents nonsense like `a > b > c > a` from happening.
(If those inequalities were all true,
transitivity would imply that `b > a`, contradicting `a > b`.)

You can tell Hode that a template is transitive (see below).
Search results can be sorted by any transitive relationship.
Some useful transitive templates include `is more important than`,
`precedes in time`, and `precedes when reading`.


# Creating transitive relationships

This is done using ordinary Hode syntax.
For instance, to indicate that the `greater-than` relationship is transitive,
we would simply add the following expression:
```
(/t /_ is greater than /_) #is transitive
```

Or, if we already know the template's address,
we can just refer to it that way:
```
(/@ 31) #is transitive
```

# Transitive search

## (Reflexive) transitive reach

Suppose your graph has the two relationships `0 #< 1` and `1 #< 2`,
and you'd like to find everything that is greater than or equal to 0.
You can find out by running the query `/f /reach 0 #< /_`.
This query can be read, "find (`/f`)
every expression that can be reached (`/reach`)
starting from `0` by following `/_ #< /_` relationships."

Similarly, you can find everything less than or equal to 2 by running
`/f /reach /_ #< 2`.

Hash treats any relationship used in a transitive search as reflexive.
Thus even though we called the relationship `#<`,
Hode acts like it is is `<=`. This is why the search
`/f /reach 0 #< /_`
includes 0 among the results.

## PITFALL: Hode believes you

Hode does not know which of your relationships "should" be transitive;
it only treats a relationship as such when you specifically tell it that the relationship is transitive.

## PITFALL: You can sneak cycles into Hode

If you mark a template as transitive before you start using it,
then Hode will prevent you from creating cycles with it.
However, before you've marked the relationship as transitive,
Hode will let you do whatever you want, including create cycles with it.

If you're not sure whether a relationship forms cycles --
for instance, not just `0 #< 1` and `1 #< 2`, but also `2 #< 0` --
then save your work before searching it transitively.

That's because Hode will crash if it encounters a cycle.
It won't respond to keypresses, and you'll need to kill it from outside.


# Sorting part of the viewtree by transitive relationships

You can sort any set of peers in a view tree by any transitive relationship.
This does not change the data in your graph,
just the way they are presented.

For instance, in a graph with the following data:
```
settlement #preceded independence
independence #preceded space travel
McCarthyism #is scary
(/t /_ preceded /_) #is transitive
```
we could sort by the `/_ preceded /_` template.

Let's get the expressions `space travel`, `settlement`, and `McCarthyism`
on the screen by running `/find settlement /| space travel /| McCarthyism`.

Those three expressions are now peers in the subgraph viewtree.
(Assuming you haven't unfolded any branches from it
(as described in [ui.md](ui.md))
the viewtree is currently just a flat list of three expressions).
We can therefore now run `/sortRight (/t /_ preceded /_)`
to sort them by the `/_ precedes /_` template.
This puts things on the right side of any transitive `/_ preceded /_`
relationship earlier (higher) in the subgraph viewtree.

Even though the graph includes no direct `/_ preceded /_`
relationship involving `settlement` and `space travel`,
they are transitively related. It is as if the graph included
`settlement #preceded space travel`.
Since we ran `/sortRight`,
and `space travel` is on the right side of that transitive relationship,
`space travel` will appear before (above) `settlement` in the viewtree.

Since `McCarthyism` is also in our list of results,
and it is not in a `/_ preceded /_` relationship with anything else on screen,
`McCarthyism` will come last in the sort.
The colors of the far left column distinguish things which are part of the sort, such as `space travel` and `settlement`,
from things which are not involved, such as `McCarthyism`.

`sl` is a synonym for `sortLeft`,
and `sr` is a synonym for `sortRight`.


# Detecting and breaking cycles

Hode will allow you to create a cycle using a transitive template.
However, once you do, Hode will announce that it has detected a cycle.
At that point, you'll have to break the cycle.

You can view the cycle in the Cycle Buffer.
(Press `M-S-b` to view all buffers.
Then move the cursor to the Cycle Buffer,
and press `M-S-r` to go to the Results Buffer.)

To break the cycle, you must delete one of the relationships in it.
Then press `M-o` to search for cycles again.
(This is necessary because when you add a relationship,
it's possible that it created more than one cycle.)
If no more cycles are found, Hode will say so.


## Example: Breaking a cycle

Consider an Rslt which contains these relationships:
```
(/t /_ x /_) #is transitive
a #x b
b #x a
```

When Hode detects the cycle (which happens as soon as it's created),
the Cycle Buffer will show something like the following:
```
address  expression
4        _ x _          -- the template
6        b
5        a
6        b
```

The way to read that is "the template `_ x _` forms the cycle `a -> b -> a`".

Notice that the Cycle Buffer only shows the expressions in the cycle,
not the relationships that join those elements.
But to break the cycle, we need to delete one of those relationships.
So let's find some of them.
Move the cursor to the first instance of the expression `b`,
and unfold the host relationships that contain it.
The viewtree should now look something like the following:

```
4  _  _
6 b
  it #x _
    10 b #x a
  _ #x it
    7 a #x b
5 a
6 b
```

(The numbers above are the addresses of the expressions they precede.
If you don't see those, press `a` while in subgraph mode to enable them.)

Now we see that the relationship `b #x a` is at address 10,
and `a #x b` is at address 7.

We can break the cycle by deleting either one.
Let's delete the one at `7`, by entering the command `/d 7`.
Now we can run `M-o` to search for cycles again.
This time Hode finds none, and allows us to return to our normal activities.


# PITFALL: Don't declare a template transitive after forming cycles in it

Hode will allow you to declare that a template is transitive *after*
creating cycles with it. Later, when you try to sort by that template,
Hode might crash.

The easiest way to avoid this problem is to tell Hode
that a template is transitive
(if in fact it is) as soon as you introduce the template.
