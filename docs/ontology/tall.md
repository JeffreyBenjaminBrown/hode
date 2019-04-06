# A (the?) tall ontology

This kind of ontology can potentially classify everything completely. Every clause could be broken down into noun phrase and verb phrase; every verb phrase, into verb and object(s); every noun phrase, into noun and adjective(s); etc.

This has the advantage of automatically grouping a lot of similar things together, and reducing the number of kinds of relationships to keep track of. Rather than know which verbs, for instance, are reified, we can assume they are all neighbors of "#do"; adjectives are clustered under "#kind"; etc.

The disadvantage is that it's hard to write, and (so far) tedious to navigate. For instance, it takes this many steps to navigate from "jbb wants to learn spanish" to "spanish":

```
  1: "jbb"
    it #do _
      11: "jbb ###do want ##obj learn #obj spanish"
        its members
          3: " _ do _ "
          1: "jbb"
          10: "want ##obj learn #obj spanish"
            its members
              5: " _ obj _ "
              6: "want"
              9: "learn #obj spanish"
                its members
                  5: " _ obj _ "
                  7: "learn"
                  8: "spanish"
```


## <noun> #do <verb> :: clause

Rather than a proliferation of relationships like "eats", "wants", etc, a single "_ #do _" relationship. Also #would, #will, #did.

The verb "to be" might deserve promotion to the same level as "do", but then there would be a parallel past, present, future, and conditional ontologies.

## #doing <verb> :: adjective

## <noun> #while|#variety adjective :: noun

## <verb> #obj <noun> :: verb
