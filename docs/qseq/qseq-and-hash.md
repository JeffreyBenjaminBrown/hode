(The parser for Qseq is as yet unwritten, but it will be a superset of what is depicted here.)

The following Qseq program, using Hash as its underlying search engine:

```
a <- /it #can dance ||| /it ##has lots #of money
b <- /forsome a0 in a: /var a0 #(lives in) New Orleans
c <- /forall b0 in b: /it #knows /var b0
```

would first create a set `a` which contains all people who can dance or who have lots of money. It would then create a set b consisting of all members of a who live in New Orleans. (Actually we could have combined those into one query.) Last it creates a collection c such that every member of c knows every member of b.
