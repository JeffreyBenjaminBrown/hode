Hode is, loosely speaking, a hypergraph editor.


# Three ways to learn about Hode

## This documentation

## Video introductions

These are not a complete guide to Hode,
but they quickly show what it's good for.
Hode has gotten slicker since these videos were made,
and it has new features (e.g., Hode handles order now),
but the basic idea remains the same.

The video on navigation is probably more interesting.
I suspect it will make sense even if you haven't watched the video on editing,
but I'm not sure.

[Video: How to navigate (search and crawl)
](https://www.youtube.com/watch?v=o6yifYdKlU0).

[Video: How to edit](https://www.youtube.com/watch?v=fuCREbf1m9k).


## The in-app help

The in-app help describes all the UI and language features of Hode.

You'll still need to use the out-of-app documentation
for a good introduction to the app --
one that covers *what* and *why*, not just *how*.
But you don't have to study the docs too thoroughly,
because once you've seen what can be done,
the in-app help can quickly reveal how to do it.

The in-app help is more up-to-date than the docs --
it might cover advanced features not otherwise documented,
and the keywords and keyboard shortcuts it describes are guaranteed to be correct and complete,
whereas the documentation outside of the app might not be.

Access the help by pressing `M-?`.
(That probably means `Alt-?`, unless you use a Mac).


# What Hode is, and how it works

Hode is an editor for higher-order data.

There are three branches to Hode:
The RSLT data structure,
the Hash language for describing subsets of an RSLT,
and the UI, which lets you use the previous two things.
Each branch is described in the [docs](docs) folder.

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

To use Hode you'll need to read about the
[Rslt in detail](docs/rslt/rslt.md).
It's a super-short document -- under 600 words.

## Hash

Hash is a language,
close to ordinary natural language,
for talking about expressions in a `Rslt`.
It offers a concise representation,
both for individual `Expr`s (expressions) in a `Rslt`,
and for queries to retrieve sets of `Expr`s.

Hash doesn't do anything. It is used only to describe parts of a graph:
The expression you'd like to add,
the subset you'd like to search for, etc.

To use Hode you'll need to
[read about `Hash`](docs/hash/the-hash-language.md).

## The UI

The UI lets you do stuff to your data
-- add to it, modify it, search for it, load and save it, etc.

To use Hode you'll need to
[read about the UI](docs/ui.md).

(Actually, no, the UI is optional.
The module [Hode.NoUI](hode/Hode/NoUI.hs)
lets you use Hode directly from GHCI, without a UI.
The NoUI module is not as complete as the UI, though -- in particular,
transitivity and order are hard to deal with outside of the UI.)


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
for some clipboard functionality.

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
(If you're a hacker, you might want to run `st <- ui`,
so that you can inspect `st` after exiting Hode.
After inspection, you can resume where you left off by running
`st <- uiFromSt st`.)
