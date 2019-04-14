See below for how to install.


# There are video introductions

The video on navigation is probably more interesting. I suspect it will make sense even if you haven't watched the video on editing, but I'm not sure.

[Video: How to navigate (search and crawl)](https://www.youtube.com/watch?v=o6yifYdKlU0).

[Video: How to edit](https://www.youtube.com/watch?v=fuCREbf1m9k).


# What this is

Hode is an editor for higher-order data.

There are four main branches to the project.
Each branch is described in the [docs](docs) folder.
They are -- and this is probably the order in which you should read about them:


## The Rslt

The `Rslt` is the most general data structure I am aware of.
It is a generalization of the graph.
(It is isomorphic to what some programmers call a "hypergraph"
-- but mathematicians claimed that term first,
and in math it means something completely different.)

A `Rslt`is a collection of `Expr`s (expressions),
each of which is either a phrase or a relationship.
The relationships can involve any number of members,
and any relationship can itself belong to other relationships.

(Actually there is one more kind of `Expr`.
See the [docs](docs) folder for details.)


## Hash

`Hash` is a language, close to ordinary natural language,
for talking about a `Rslt`.
`Hash` offers a concise representation both of individual `Expr`s (expressions, i.e. members) of a `Rslt`
and for queries meant to retrieve subsets of a `Rslt`.


## The UI

It lets you do stuff
-- insert data, search for data, view data, save data, load data.


## Qseq

(`Qseq` hasn't made it's way into the UI yet,
but it's implemented.)

This is a metalanguage for search.
It lets you combine multiple searches in some underlying search language.


# How to install

First you'll need `Stack` (the Haskell toolkit) installed.
You'll also need `xsel`.

You might need a few more things I can't remember
(`libxrandr-dev`, `lbxss-dev` and `libx11-dev`, maybe?).
If you really do, Stack will let you know.

Clone the repo,
then run `stack run` from the command line.
The first time you run it,
it will take a long time to load.


# Hode does not back up your data. I recommend Git for that.

Hode has a simple mechanism for saving to disk and reading from disk.
It has, however, no backup mechanism and no safety features
-- if you ask it to overwrite a big beautiful graph with a tiny ugly one,
it will oblige.

Hode saves every expression as a separate text file.
Thus if you use Git (or some other version control system),
the diffs will be readable.
