## regex-derivatives

Simple regex engine written in Haskell.

Instead of using backtracking or finite automata to match strings, it uses derivatives.  The idea and implementation are taken from a blog post by [Matt Might][Matt's blog].

To install regex-derivatives, run `cabal update && cabal install` from inside of the 'regex-derivatives' directory.

To use regex-derivatives, run `regex pattern text`, i.e. `regex 'Str..' "$(cat ./Main.hs)"`.

The output is colored like the output of [ag][the_silver_searcher].

[Matt's blog]: http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
[the_silver_searcher]: https://github.com/ggreer/the_silver_searcher
