-- works:
>  parseTest expr "a #is ##for b"

Open 2
  [ Open 1 [ Leaf "a"
           , Absent ]
    [ "is" ]
  , Leaf "b" ]
  ["for"]


-- almost works:
> parseTest expr "a #is ##for b #to thing ##because #maybe yeah"

Open 2
  [Open 1
    [Absent,Leaf "yeah"]
    ["maybe"],Open 1
              [Leaf "a",Absent]
              ["is"],Open 1
                     [Leaf "b",Leaf "thing"]
                     ["to"]]
  ["because","for"]
