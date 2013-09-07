(defproject net.publicfields/scribble "0.1.0-SNAPSHOT"
  :description "A Racket's Scribble sub-language implementation in Clojure"
  :url "http://github.com/Manticore/clojure-scribble"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [one_more_minute/clarity "0.1.1"]]
  :dev-dependencies [[lein-kibit "0.0.8"]
                     [jonase/eastwood "0.0.2"]
                     [lein-marginalia "0.7.1"]
                     [lein-cloverage "1.0.2"]]
  :main scribble.core)
