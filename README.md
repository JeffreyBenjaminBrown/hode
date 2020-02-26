Skip to the end of this README for how to install.

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

A `Rslt` is a data structure that generalizes the graph.
It can easily represent any natural language expression.
(The `Rslt` is isomorphic to what some programmers call a "hypergraph" --
but mathematicians claimed that term first,
and in math it means something much less general.)

A `Rslt`is a collection of expressions,
each of which is either a phrase (like "cats"),
or a relationship (like "cats have noses")
or a template (like "_ have _") that relationships share.

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
but if you're a curious hacker, it's implemented.)

[Qseq](Hode/Qseq/) is a metalanguage for search.
It lets you use existential and universal quantifiers to string together searches in some underlying search language.

# How to install

Hode is unavailable for Windows. (That's because it relies on the [Brick](https://hackage.haskell.org/package/brick) terminal interface library.)

## Prerequisites

Before installing Hode, you'll need to first have installed

* [`Stack`](https://docs.haskellstack.org/en/stable/README/), the Haskell toolkit

* [`Git`](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git), the version-control software

* Optionally, you can install [`xsel`](https://linux.die.net/man/1/xsel)
for trafficking to and from the clipboard without using the mouse.

## Install Hode

Open a console (a.k.a. "terminal", "command line", "command prompt").
Clone the repo, enter it, and start ghci:
```
git clone https://github.com/JeffreyBenjaminBrown/hode
cd hode
stack ghci
```
The first time you try that,
it will take a long time to start.

Depending on your system, Stack might complain that
you need a few more things installed.
(`libxrandr-dev`, `lbxss-dev` and `libx11-dev`, maybe?).
If it does, install those too.
Then return to the cloned repo and try `stack ghci` again.

Once that works and you're in GHCI, run `ui`.
