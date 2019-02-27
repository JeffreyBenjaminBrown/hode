# The UI

The UI lets you load, save, add to, and search for data. It also offers a few keyboard shortcuts for moving text around, which you'll want to do because the text editor in the UI is minimal.

Switch focus between the two windows using the Tab key. The top one is for displayuing results, and the bottom one is for entering commands.


## Starting it

The easiest way to start the UI is with an empty `Rslt`, by calling ` ui $ mkRslt mempty` from inside GHCI.


## Copy focused window's entire contents to clipboard: Alt-w

(At least, Alt is the modifier key to use on my system. YMMV.)


## Delete all text in the command window: Alt-k


## Execute a command: Alt-x

First type the command in the control window, then type Alt-x.

Errors are displayed. Success is silent, except for successful `/find` commands, which display something in the results window.

These are the commands you can execute:


### Load a Rslt from disk

Type something like `/load folder/subfolder/subfolder`. Point it at a folder, not a file: The Rslt is saved as a lot of tiny `.rslt` files, so it'll need a whole directory to put them in. Write the in absolute terms or relative to wherever you started GHCI from.


## Save a Rslt to disk

`/save folder/subfolder/subfolder`


## Add an expression

The documentation on the Rslt and Hash provide details; here's a refresher:

`/add dogs`

`/add /hash dogs #eat grass`

`/add /addr 1 #because /addr 2`

`/add /addr 1 #because /addr 2 ##(and therefore) /addr 3`


## Search for a Hash expression

See the documentation on Hash for details. Here's an incomplete refresher:

`/find bob`

`/find /addr 1`

`/find bob #likes pizza with pineapple ##because _`

`/find bob #likes pizza with pineapple ##because /it`
  -- Returns only the reason, not the full "because" relationship.

`/find bob #likes _ || bob #dislikes _`
  -- Every #likes and every #dislikes statement with bob on the left.
