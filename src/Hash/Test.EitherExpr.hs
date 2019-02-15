-- works
> > parseTest expr "a b #(w x) c d"
Open 1 [Leaf "a b",Leaf "c d"] ["w x"]

-- works
> parseTest expr "a #is ##for b"
Open 2
  [ Open 1 [ Leaf "a"
           , Absent ]
    [ "is" ]
  , Leaf "b" ]
  ["for"]

-- works
> parseTest expr "a #is ##for b #to thing ##because #maybe yeah"

Open 2
  [Open 1
    [Leaf "a",Absent]
    ["is"]
  ,Open 1
    [Leaf "b",Leaf "thing"]
    ["to"]
  ,Open 1
    [Absent,Leaf "yeah"]
    ["maybe"]]
  ["for","because"]

> parseTest expr "a #is ##for (b #to thing ##is helpful) ##because #maybe yeah"
( Open 2 [ Open 1
           [ Leaf "a"
           , Absent]
           [ "is"]
       , Closed [ Open 1
                  [ Leaf "b"
                  , Leaf "thing"]
                  [ "to"]
                , Leaf "helpful"]
         [ "is"]
       , Open 1 [ Absent
                , Leaf "yeah"]
         [ "maybe"]]
  ["for","because"]
  )
