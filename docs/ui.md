You don't need to remember all these commands!
They're easy to find in the in-app help, which you can open by running
`M-?` (which probably means `Alt ?`, unless you use a Mac).
Read this just to get a sense of what's possible.

In fact, this documentation might go stale,
describing keyboard shortcuts that don't work.
That is less likely for the in-app help, which is somewhat self-updating.

# The UI

The UI lets you load, save, add to,
search for, and navigate your data,
as well as choose how it is displayed.

The text editor in Hode is extremely simple.
To enter a complex expression into Hode,
I prefer to write it in another application (Emacs),
then copy and paste it into Hode.

## Note: Hode does not back up your data.

If you ask Hode to overwrite a big beautiful graph with a tiny stupid one,
Hode will obey.

I suggest using version control software to back up your data.
Hode uses a simple,
human-readable format for saving to and reading from disk.
This means it works well with Git, or anything like it.
Each expression corresponds to a unique file,
making the diffs between versions of your data readable.

# To start the UI

## The simplest way to start the UI

Call `stack run` from the command line.

## A more flexible way to start the UI

Call `stack ghci` from the command line.
Now, from within GHCI, you can:

* Call `st <- ui` to start the UI. This way, once you exit the UI,
`st` will be an object of type `St` (the state of the app),
which you can inspect and mess with.

* Call `uiFromSt st` from GHCI, where `st` has type `St`,
to provide your own initial state to the app.

* Call `uiFromRslt r`, where `r` has type `Rslt`,
to provide your own initial graph to the app,
and otherwise use all the default values.

# What you see (and how to change the way things are shown)

## Windows, buffers, modes and submodes

I have tried to use Emacs terms as much as possible.

The app always shows the `main window`,
and sometimes at the bottom, it also shows the `command window`.

Whenever the command window is showing, you are in `command mode`.
This is true regardless of what is shown in the main window.

If the command window is not showing,
then the `mode` you are in is determined by what is in the `main winndow`.
For instance, if the `main window` is showing the `subgraph buffer`,
and the `command window` is hidden, then you are in `subgraph mode`.

Together, the current `mode` and `submode`
determine what commands are available.
Not every `mode` has submodes.
For instance, currently the `subgraph mode` has two `submode`s:
`primary` and `order` (described below),
but the other `mode`s don't have `submode`s.
Although this document might go stale,
you can always check the interactive help (by pressing `M-?`)
for an up-to-date list of available modes and submodes.

## Using the command window

Show or hide the command window with `M-c`
(i.e. probably `Alt-c`, unless you use a Mac).

When the command window is showing,
anything typed without using a special modifier key
will modify the text in the command window.
Once you have typed a command,
execute it by pressing `M-x`.

A subset of the Emacs (and Bash)
keyboard shortcuts are available in the command window.
Press `C-a` (that's probably `Control-C`)
to put the cursor at the front of the line.
Press `C-k` to delete everything at or after the cursor.

## The main window

The main window can show a few different kinds of things.

Most of the time,
the main window will display a `subgraph buffer`.
There can be multiple `subgraph buffer`s open at the same time,
each (probably) corresponding to a different search.
Whatever the main window is currently showing,
you can switch to the current `subgraph buffer` by pressing `M-g`.

Sometimes you wiill want to switch between subgraph buffers.
Switch to the `buffer select buffer` to do that.
The `buffer select buffer` is described below.

When an error happens,
the main window will automatically switch to the `error buffer`.
There are lots of ways to escape the error buffer --
for instance, by switching to the subgraph buffer,
as described above.

## The subgraph buffer

A subgraph buffer shows a subset of your data.
It's initially populated by running a search (something starting with `/find`)
in the command window.

### The things on the right side are expressions

The UI displays expressions from your graph using
[the Hash language](hash/the-hash-language.md).

When you first run a search,
everything it finds is displayed left-justified.
Thereafter you can use keyboard commands
(e.g. `insert hosts`, described later in this document)
to complicate that view,
inserting things that are justified farther to the right.

Here's an example of a subgraph buffer's contents,
after such complication.
(The actual display is in color, which makes it more readable.
The column of numbers at the far left is described in the next section.)

```
3 6 haskell
   _ #of it
1     7 stack #of haskell
1     22 extension #of haskell
   _ #using it
1     89 benchmark #using haskell
0 666 java
```

#### The address to the left of each expression

Each expression is stored at an `address` in the graph.
By default, that address is displayed to the immediate left of the expression itself, in a different color.
For instance, in this example,
the expression "benchmark #using haskell" is at address 89.

When you're in `subgraph mode` (i.e. you've hidden the command window),
those addresses can be toggled off and on by pressing `a`:

```
3 haskell
   _ #of it
1     stack #of haskell
1     extension #of haskell
   _ #using it
1     benchmark #using haskell
0 java
```

#### Grouping

Note that some of the rows of the example display have no number in the column at the far left.
That is because they are not expressions in the graph.
They are used, rather, to group the expressions below them.
Where they say `it`, `it` refers to their immediate parent --
e.g. in the example above, `it` means "haskell"e.

#### Redundancy

If "haskell" was a much longer phrase,
you might wish that the display did not repeat it.
In `subgraph mode`,
you can toggle whether descendents restate the expressions they are descended from by pressing `A`.
For instance, the above example would become this:

```
3 6 haskell
   _ #of it
1     7 stack #of @6
1     22 extension #of @6
   _ #using it
1     89 benchmark #using @6
0 666 java
```

Rather than "haskell",
the formerly redundant subexpressions now read "@6" --
that is, "the expression at address 6".

(Note that if you're reducing redundancy this way,
you probably don't want to have disabled the display of addresses to the left of expressions.)

### Columns

#### The hosts count

There's a column at the left with numbers in it.
For each expression at the right,
this column indicates how many expressions contain that one.
For instance, if your data includes "cats",
and "cats #eat bugs", and "cats #avoid water" (and nothing else),
then the number 2 would appear in the hosts count next to "cats",
because two expressions contain it.

#### Adding more columns

By default, the hosts count is the only column shown.

If you want to hack around, you can change that.
Just change the `_columnHExprs` field in the app's `St`.

# Language commands vs. keyboard commands

Commands in this application divide neatly into two categories.
`Language commands` require you to type a statement into the command window,
and then execute the statement.
`Keyboard commands` resemble using the cursor keys in a text editor.

Language commands can be executed from anywhere.
The keyboard commands available depend on what is shown in the main window.

# Language commands

These are entered in the command window, and executed with `M-x`.

## Load a Rslt from disk

Type something like `/load folder/subfolder/subfolder`.
(It has to start with the symbol `/load`, followed by a space.)
If there are `.rslt` files in the path you entered,
they will be loaded into the graph.
Anything that was already in it will disappear.

Write the path in absolute terms,
or relative to wherever you started GHCI from.

## Save a Rslt to disk

Same idea:
`/save folder/subfolder/subfolder`

## Replace an expression

Write `/replace` (or `/r`),
followed by the expression's address,
followed by what should be there.

For instance, `/replace 3 x # y` would replace
whatever used to be at address 3 with the expression `x # y`.

### Note: Replacement-induced deletion is not recursive

Replacement destroys the old expression.
Any "host expression" (or "superexpression")
that used to contain it
now contains the new expression instead.

Replacement does *not*, however,
destroy the old expression's sub-expressions.

This has security implications.
For instance, if your graph used to contain the statement,
"I #enjoy pornography", and you replaced that with
"I #enjoy ethnography", your graph still contains
"pornography".

## Delete an expression

Deletion is only possible for a top-level expression -- that is, an expression that is not a member of any other expressions. (If you deleted "seaweed", what would happen to "I #(must buy) seaweed"?)

The syntax is `/delete a` or `/d a`, where "a" is the address of the expression to be deleted.

## Add an expression

The documentation on the Rslt and Hash provide details.
Here's a brief refresher:

`/add dogs`

`/add dogs #eat grass`

`/add /addr 1 #because /addr 2`

`/add /addr 1 #because it's cheap ##(and therefore) /addr 3`

## Search for a Hash expression

See the documentation on Hash for details.
Here's a brief refresher:

`/find bob`

`/find /addr 1`

`/find bob #likes (pizza #with pineapple) ##because /_`

`/find /eval bob #likes pizza ##with pineapple ###because /it)`
  -- Returns only the reason, not the full "because" relationship.

`/find bob #likes /_ || bob #dislikes /_`
  -- Every #likes and every #dislikes statement with bob on the left.

## Sort the peers of the currently focused node

See the [documentation on order](order.md).

## Move an expression (change its address)

This is kind of a strange thing to do.
You probably won't ever need to.
However, just in case:

To move the expression at address `a` to address `b`, type this:

`/move a b`.

# Keyboard commands

## Keyboard commands that work from multiple places

### Quit: `M-esc`

### Move focus: `M-e` (up), `M-f` (right), `M-d` (down), `M-s` (left)

One result in the subgraph buffer is always focused.
Similarly, one buffer in the `Select Graph Buffer` Buffer is always focused.
These move that around.

PITFALL: There are two separate kinds of focus.
Every subgraph buffer has a focused result;
it's the one with a different color scheme.
Exactly one of the subgraph buffers is itself focused;
it's the buffer with a different color scheme in the `Select Graph Buffer` Buffer.

### `M-x`: Execute command

Once you've typed something into the Command window,
do this to run it.

### Switch the kind of buffer shown

Show (the currently focused) subgraph buffer: `M-r`

Show (the) `Select Graph Buffer` buffer: `M-b`

Show (the) command history buffer: `M-h`

Show (the) error buffer: `M-e`

## Keyboard commands that work from the subgraph buffer

### `M-h`: Insert hosts at focus

The focused result in the subgraph buffer might be a member of other relationships.
If it is, this displays those relationships "under" it
(that is, below it and slightly to its right).

### `M-m`: Insert members at focus

The focused result, if it is a relationship, consists of members.
This displays each of those members under it.

### `M-c`: Close whatever is displayed underneath the focused result

Repeated use of the previous two commands can lead to a very cluttered view.
This can clean it up.

### `M-b`: Switch to a new buffer rooted at the focused result

This creates a new subgraph buffer showing exactly one thing,
the result that was focused in the previous buffer.
The previous subgraph buffer continues to exist,
and in the `Select Graph Buffer` buffer,
the new subgraph buffer is a child of the old one.

### `M-S`: Insert results of evaluating focus as a search

#### The idea

Suppose you often find yourself running

`/find alice | bob | chris`

You can add that search as an expression to your graph.
Once it's in your graph, when it has focus
(see the section called "Move focus" earlier),
you can run it by pressing `M-S`.
The search results will be inserted underneath it.

#### Encoding a search: the fragile way

There are many ways you could add that search as an expression in the graph. Here is the simplest way:

`/add "alice | bob | chris"`

Note that the query has been wrapped in quotation marks. Otherwise Hode would try, as usual, to interpret the `|` symbols to mean "or", and it would get confused. After running the above expression, your graph will contain the expression

`"alice | bob | chris"`.

#### Why that way is fragile

Suppose your graph had an error, and you had to change "bob" to "rob".
Every expression that "bob" was in now says "rob" instead.
But the search you encoded did not include "bob" --
it contained instead the string "alice | bob | chris".
So now the search is broken --
it will continue looking for "bob", not "rob".

#### Econding a search: the robust way

Here is a better way to encode the same search:

`/find alice # "|" # bob # "|" # chris`

Each term in the search -- the words and the `|` symbols --
are now separated by `#` marks. When you run the search,
the expression is "flattened": the # marks are stripped away,
and the results joined together (with spaces in between).

This way, when you press `M-S`, the search that gets run is the same:

`alice | bob | chris`

But since each term in the search has been encoded as a separate entity,
Hode "knows what they are". If you change "bob" to "rob",
it will change in the search too.

(You don't have to wrap "alice" or "bob" or "chris" in quotation marks,
because they contain no characters that Hode treats specially.)

### `M-w`: Copy the subgraph buffer to clipboard

This copies everything in the currently-displayed subgraph buffer,
even if it does not all fit on the screen.

### `M-r`: Replace the command window with this buffer's last successful search

## Keyboard commands that work from the "select subgraph" buffer

From the buffer window you can create new buffers
and switch between them by moving focus (as described above).
Once you've moved focus to the buffer you'd like to see,
you can switch back to the subgraph buffer with `M-S-r`.

### `M-t`: Create new empty top-level buffer

### `M-c`: Create new empty buffer as a child of the currently focused buffer
