# clj-selectors

**This was my first toy clojure project and it is in a partially working state.** Leaving it here in case anyone wants to mess with it, but I am not using this.

A Clojure implementation of [CSS Selectors](http://www.w3.org/TR/css3-selectors/). These can be used to match element trees produced by [clj-tagsoup](https://github.com/nathell/clj-tagsoup).

## Usage

The most basic way to use this library is to pass a string selector to the `$` function along with a parsed element tree:

```clojure
(let [tree (pl.danieljanus.tagsoup/parse "http://www.nasdaq.com/symbol/etsy")]
  ($ "#qwidget_lastsale" tree))

;; --> ([:div {:class "qwidget-dollar", :id "qwidget_lastsale"} "$8.86"])
```

Selectors can also be precompiled for later use:

```clojure
(def a-compiled-selector (compile-selector "foo .bar #baz"))
($ a-compiled-selector tree)
```

## License

Copyright © 2015 [Dan McKinley](http://www.mcfunley.com")

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
