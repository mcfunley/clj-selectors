(defproject clj-selectors "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles { :test { :dependencies [[clj-tagsoup/clj-tagsoup "0.3.0" :exclusions [org.clojure/clojure]]]
                     }}
  :global-vars {*warn-on-reflection* true})
