# The UI

The UI lets you load, save, add to, and search for data.
It also offers a few keyboard shortcuts for moving text around,
which you'll want to do because the text editor in the UI is minimal.


# Starting the UI

Call `stack run` from the command line,
or call `ui` from inside GHCI.
(Or if you're feeling especially fancy,
you can call `uiFromSt st` from within GHCI,
where `st` has type `St`,
to provide your own initial state to the app.)


# Windows and buffers

I have tried to borrow Emacs terms as much as possible.

The app is divided into a main window and a command window.
Any time you type without using a special modifier key (e.g. `Alt`),
you modify the text in the command window.
Once you have typed a command worth executing,
you can execute it by pressing `M-x` (i.e., probably, `Alt x`).

Most of the time,
the main window will display a `results buffer`.
There can be multiple buffers open at the same time,
each corresponding to a (probably) different search.
Whatever the main window is currently showing,
you can switch to the current results buffer by pressing `M-S-r`
(that is, probably, `Alt + Shift + r`).

Sometimes you wiill want to show all the results buffers at once.
Switch to the `buffer buffer` to do that, with `M-S-b`.
The buffer buffer is described below.

When an error happens,
the main window will automatically switch to the `error buffer`.
You can also manually switch to the error buffer with `M-S-e`.
(There are lots of ways to escape the error buffer --
for instance, by switching to the results buffer,
as described above.)


# Language commands vs. keyboard commands

The human brain has been supposed to divide neatly into a language half and a gemoetric half.
That's actually nonsense,
but it's really true of commands in this application.
Some commands require you to type a statement into the command window,
and then execute the statement.
Other commands resemble using the cursor keys in a text editor.

I will call the first kind `language commands` and the second kind `keyboard commands`. Note that while language commands can be executed from anywhere, keyboard commands depend on what is shown in the main window.


# Language commands

These are entered in the command window, and executed with `M-x`.

## Load a Rslt from disk

Type something like `/load folder/subfolder/subfolder`. Point it at a folder, not a file: The Rslt is saved as a lot of tiny (human-readable) `.rslt` files, so it'll need a whole directory to put them in. Write the path in absolute terms, or relative to wherever you started GHCI from.


## Save a Rslt to disk

`/save folder/subfolder/subfolder`


## Add an expression

The documentation on the Rslt and Hash provide details; here's a refresher:

`/add dogs`

`/add dogs #eat grass`

`/add /addr 1 #because /addr 2`

`/add /addr 1 #because /addr 2 ##(and therefore) /addr 3`


## Search for a Hash expression

See the documentation on Hash for details. Here's an incomplete refresher:

`/find bob`

`/find /addr 1`

`/find bob #likes (pizza #with pineapple) ##because /_`

`/find /eval bob #likes pizza ##with pineapple ###because /it)`
  -- Returns only the reason, not the full "because" relationship.

`/find bob #likes /_ || bob #dislikes /_`
  -- Every #likes and every #dislikes statement with bob on the left.


# Keyboard commands

## Keyboard commands that work from multiple places

### Quit: `M-esc`

### Move focus: `M-e` (up), `M-f` (right), `M-d` (down), `M-s` (left)

One result in the results buffer is always focused.
Similarly, one buffer in the buffer buffer is always focused.
These move that around.

PITFALL: There are two separate kinds of focus.
Every results buffer has a focused result;
it's the bit that shows in green when you're looking at that results buffer.
Exactly one of the results buffers is itself focused;
it's the buffer that shows in green when you look at the buffer buffer.


### `M-x`: Execute command

### `M-k`: Delete all text in the Command window

### Switch the kind of buffer shown

Show (the currently focused) results buffer: `M-r`

Show (the) buffer buffer: `M-b`

Show (the) error buffer: `M-e`


## Keyboard commands that work from the Results buffer

### `M-h`: Insert hosts at focus

The focused result in the results buffer might be a member of other relationships.
If it is, this displays those relationships "under" it
(that is, below it and slightly to its right).

### `M-m`: Insert members at focus

The focused result, if it is a relationship, consists of members.
This displays each of those members under it.

### `M-c`: Close whatever is displayed underneath the focused result

Repeated use of the previous two commands can lead to a very cluttered view.
This can clean it up.

### `M-b`: Switch to a new buffer rooted at the focused result

This creates a new results buffer showing exactly one thing,
the result that was focused in the previous buffer.
The previous results buffer continues to exist,
and in the buffer buffer,
the new results buffer is a child of the old one.

### `M-w`: Copy the results buffer to clipboard

This copies everything in the currently-displayed results buffer,
even if it does not all fit on the screen.

### `M-r`: Replace the command window with this buffer's last successful search


## Commands that work from the buffer buffer

From the buffer window you can create new buffers and switch between them by moving focus (as described above).
Once you've moved focus to the buffer you'd like to see,
you can switch back to the results buffer with `M-S-r`.

### `M-t`: Create new empty top-level buffer

### `M-c`: Create new empty buffer as a child of the currently focused buffer
