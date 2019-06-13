I wrote some code that defines a `String`-like type that can have different attributes (Graphics.Vty.Attr values) along different portions:

```
type AttrString = [(String, V.Attr)]
```

and then a function which wraps an `AttrString` around the space allocated to the resulting `Widget`:

```
attrStringWrap ::  AttrString -> Widget n
attrStringWrap ss =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLines w ss
    return $ Result i [] [] [] B.empty

  where

  linesToImage :: [AttrString] -> V.Image
  linesToImage = let g (s,a) = V.string a s
    in V.vertCat . map (V.horizCat . map g)

toLines :: Int -> AttrString -> [AttrString]
toLines maxWidth = reverse . map reverse . f 0 [] where
  f _       acc               []                  = acc
  f _       []                ((s,a):moreInput)   =
    f (length s) [[(s,a)]] moreInput
  f lineLen o@(line:moreOutput) ((s,a):moreInput) =
    let newLen = lineLen + length s
    in if newLen > maxWidth
       then f (length s) ([(s,a)]     :o)          moreInput
       else f newLen     (((s,a):line):moreOutput) moreInput
```

Then I wrote a function

```
showTwoAspects :: forall a b n.
     (b -> Widget n)
  -> (a -> b) -- ^ shows one aspect of an `a`
  -> (a -> b) -- ^ shows another
  -> [a]      -- ^ what to show
  -> Widget n
showTwoAspects b2w showLeft showRight =
  vBox . map showRow
  where
    showRow :: a -> Widget n
    showRow a = hBox [ b2w $ showLeft a
                     , b2w $ showRight a ]
```

If I provide `attrStringWrap` as the first argument to `showTwoAspects`, I get strange behavior: The text wraps correctly within the allocated space, but that space only occupies about half of the width of the screen, whereas I expected it to run all the way to the right.

If I change the definition of `showTwoAspects` so that it does not use an `hBox`, and does not draw the left portion, the behavior is as expected. `showOneAspect` does that:

```
showOneAspect :: forall a b n.
     (b -> Widget n)
  -> (a -> b) -- ^ ignored
  -> (a -> b) -- ^ shows an aspect of a
  -> [a]      -- ^ what to show
  -> Widget n
showOneAspect b2w _showLeft showRight =
  vBox . map showRow
  where
    showRow :: a -> Widget n
    showRow a = b2w $ showRight a
```

All the code described above is at `Hode.Lib`. Some functions that demonstrate how `showOneAspect` succeeds and `showTwoAspects` fails can be found at `Hode.Test`. An alternative (but as far as I can tell, equivalent) definition of `attrStringWrap`, and corresponding tests, can be found at `Hode.Test2`.
