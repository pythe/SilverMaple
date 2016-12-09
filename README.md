# SilverMaple
An elm module for scoring string matches, like LiquidMetal.js.
Queries including the target's capitals or word starts are scored higher than
queries that only match lower-case, interior letters.

```
SilverMaple.score("SM", "SilverMaple") -> 10.1f
SilverMaple.score("re", "SilverMaple") -> 4.3f
```


# My understanding of the algorithm

`score`
1. Iterate through the query. For each character in the target, if it matches:
  * score a Match and ...
  * If the match `isStarting`, score a Match for the previous character and fill the remaining scores with SpecialTrailing.
  * Else, fill the remaining scores with Trailing.
1. Return the sum of the scores.
1. Return the maximum score.

`scoreAll`
1. Given
