See below for how to install.

# There are video introductions

These were made in early 2019.
The app has gotten much slicker since,
but the basic idea remains the same.

The video on navigation is probably more interesting.
I suspect it will make sense even if you haven't watched the video on editing,
but I'm not sure.

[Video: How to navigate (search and crawl)
](https://www.youtube.com/watch?v=o6yifYdKlU0).

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
each of which is either a phrase,
or a relationship between phrases,
or a "template" that describes the structure that a set of relationships have in common.

What distinguishes a `Rslt` from the a `graph`
is that the relationships can involve any (positive) number of members,
and any relationship can itself belong to other relationships.

It only takes about 500 words to describe a
[Rslt in detail](docs/rslt/rslt.md).

## Hash

[`Hash`](docs/hash/the-hash-language.md) is a language,
close to ordinary natural language,
for talking about a `Rslt`.
`Hash` offers a concise representation,
both for individual `Expr`s (expressions) in a `Rslt`,
and for queries which retrieve subsets of a `Rslt`.

## The UI

[The UI](docs/ui.md) lets you do stuff
-- insert data, search for data, view data, save data, load data.

## Qseq

(This hasn't made it's way into the UI yet,
but it's implemented.)

[Qseq](Hode/Qseq/) is a metalanguage for search.
It lets you combine multiple searches in some underlying search language.

# How to install

First you'll need the Haskell toolkit [`Stack`](https://docs.haskellstack.org/en/stable/README/) installed.
If you want to be able to traffic to and from the clipboard using the keyboard
(which can be handy if your data does not fit on screen at once)
you'll also need to have `xsel` installed, too.

You might need a few more things I don't remember
(`libxrandr-dev`, `lbxss-dev` and `libx11-dev`, maybe?).
If so, Stack will let you know.

Clone the repo,
then run `stack run` from the command line.
The first time you run it,
it will take a long time to load.

# Hode does not back up your data.

Hode uses a simple,
human-readable format for saving to and reading from disk.
This means it works well with Git,
or any other version-control software.
Each expression corresponds to a unique file,
making the diffs between versions of your data readable.

I suggest using such software to backup your data.

If you ask Hode to overwrite a big beautiful graph with a tiny stupid one,
Hode will obey.
