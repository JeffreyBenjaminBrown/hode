# Background: Order, transitive relationships, and cycles

A transitive template `t` satisfies the property that
```
a #t b & b #t c => a #t c
```

For instance, "greater than" relationships are transitive:
If a is greater than b, and b is greater than c, then a is greater than c.

Transitive templates cannot form cycles.
(If it formed a cycle, Hode could not sort the data.)

Templates in Hode can be marked as transitive.
Search results can be sorted by any transitive relationship.
Some transitive templates I find useful include "is more important than",
"preceded (in time)", and "should precede when reading".


# Creating transitive relationships

This is done using ordinary Hode syntax.
For instance, to indicate that the "greater-than" relationship is transitive,
we would simply add the following expression:
```
(/t /_ is greater than /_) #is transitive
```


# Sorting by transitive relationships

You can sort by any transitive relationship.
Therefore, in a graph with the following data:
```
frogs #eat bugs
bugs #outrun frogs
(/t /_ eat /_) #is transitive
```
we can sort by the "eat" relationship.
(Note that "eat" is not truly a transitive relationship.
For one thing, snakes eat frogs and frogs eat snakes.
For another thing, eagles eat mice and mice eat grains,
but eagles don't eat grains. Still, I'm going to use the example.)

Let's get frogs and bugs on the screen by running `/f frogs | bugs`.
If we now move the cursor over one of them
(see "move focus" under "keyboard commands" for how to do that),
we can now run `/sortLeft (/t /_ eat /_)` to sort by the "eat" relationship,
with things on the left side of the relationship shown earlier in the results.
Since `frogs #eat bugs`, frogs are left of bugs in the eat relationship,
so frogs will precede bugs in the list.

If trees was also in our list of results,
and trees was not in an eat relationship with either frogs or bugs,
trees would come last in the sort.

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
If none are found, Hode will say so.

## Example: Breaking a cycle

Consider an Rslt which contains these relationships:
```
a #x b
b #x a
(/t /_ x /_) #is transitive
```

The Cycle Buffer will show us something like the following:

```
4  _ x _
6 b
5 a
6 b
```

(I have omitted the columns on the left, 
as they are hard to read in black and white.
The first number in each row is the expression's address.)

The way to read that is "the template `_ x _` forms the cycle `a -> b -> a`.

Notice that it only tells us the expressions in the cycle, 
not the relationships that join those elements.
But to break the cycle, we need to delete one of those relationships.
So let's find some of them. 
Move the cursor to the first instance of the expression `b`,
and unfold the host relationships that contain it (`M-h`):

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

Now we see that the relationship `b #x a` is at address 10,
and `a #x b` is at 7.

We can break the cycle by deleting either one.
Let's delete the one at 7, by entering the command "/d 7".
Now we can run `M-o` to search for cycles again.
This time Hode finds none, and allows us to return to our normal activities.
