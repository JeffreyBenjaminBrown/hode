Skip to the end of this README for how to install.


# There are video guides

The app has gotten much slicker since these were made in early 2019,
and it has new features,
but the basic idea remains the same.

The video on navigation is probably more interesting.
I suspect it will make sense even if you haven't watched the video on editing,
but I'm not sure.

[Video: How to navigate (search and crawl)
](https://www.youtube.com/watch?v=o6yifYdKlU0).

[Video: How to edit](https://www.youtube.com/watch?v=fuCREbf1m9k).


# There is in-app help

And it's more up-to-date --
it includes some features not otherwise documented,
and the keywords and keyboard shortcuts it describes are guaranteed to be correct,
whereas the documentation outside of the app might not be.

You'll still need to use this out-of-app documentation
for a good introduction to the app.
But you don't have to study it too hard,
because once you've seen *what* can be done,
the in-app help can quickly reveal *how* to do it.

Access the help by pressing `M-?`.
(That probably means `Alt-?`, unless you use a Mac).


# What Hode is, and how it works

Hode is an editor for higher-order data.

There are three branches to Hode:
The data structure, the language for talking about it,
and the UI for using them.
Each branch is described in the [docs](docs) folder.

(The UI is recommended but actually optional. The module
[Hode.NoUI](hode/Hode/NoUI.hs)
is designed to allow someone to use Hode directly from GHCI, without a UI.
NoUI is not as complete as the UI, though -- in particular,
transitivity and order are hard to deal with outside of the UI.)

## The Rslt data structure

A Rslt (Recursive Set of Labeled Tuples)
is a generalization of a knowledge graph.
It lets a user easily represent any natural language expression.
(A `Rslt` is isomorphic to what some programmers call a "hypergraph" --
but mathematicians claimed that term first,
and in math it means something much less expressive.)

A `Rslt`is a collection of expressions,
each of which is either a phrase (like "cats"),
or a relationship (like "cats have noses")
or a template (like "_ have _") shared by many relationships.

What distinguishes a `Rslt` from a `graph`
is that relationships can involve any (positive) number of members,
and a relationship can itself belong to other relationships.

Hode will make much more sense once you've read about the
[Rslt in detail](docs/rslt/rslt.md).
(That's a super-short document -- under 600 words.)

## Hash

Hash is a language,
close to ordinary natural language,
for talking about expressions in a `Rslt`.
It offers a concise representation,
both for individual `Expr`s (expressions) in a `Rslt`,
and for queries to retrieve sets of `Expr`s.

To use Hode you'll need to
[read about `Hash`](docs/hash/the-hash-language.md).

## The UI

The UI lets you do stuff
-- insert data, search for data, view data, save data, load data.

To use Hode you'll need to
[read about the UI](docs/ui.md).

## You can ignore Qseq

The fourth branch of Hode,
[Qseq](Hode/Qseq/) is a metalanguage for search.
It lets you use existential and universal quantifiers to string together searches in some underlying search language.

If you're a curious hacker, Qseq has been implemented,
but it hasn't made it's way into the UI yet.


# How to install

## If you run Windows, you'll need to run Linux inside it

That's because Hode relies on the
[Brick](https://hackage.haskell.org/package/brick)
terminal interface library.
There are, fortunately, lots of ways to run Linux in Windows
-- Docker, for instance.

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
