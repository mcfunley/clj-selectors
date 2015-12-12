# clj-selectors

A Clojure implementation of CSS selectors. These can be used to match element trees produced by [clj-tagsoup](https://github.com/nathell/clj-tagsoup).

## Usage

```clojure
(let [tree (pl.danieljanus.tagsoup/parse "http://www.nasdaq.com/symbol/etsy")]
  ($ "#qwidget_lastsale" tree))

;; --> ([:div {:class "qwidget-dollar", :id "qwidget_lastsale"} "$8.86"])
```

## License

Copyright © 2015 Dan McKinley

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
