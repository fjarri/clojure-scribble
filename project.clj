(defproject net.publicfields/scribble "0.1.0-SNAPSHOT"
  :description "A Racket's Scribble sub-language implementation in Clojure"
  :url "http://github.com/Manticore/clojure-scribble"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.7.6"]
                 [chiara "0.2.0-SNAPSHOT"]]
  :profiles {
    :dev {
      :dependencies [[midje "1.5.1"]]
      :plugins [
        [lein-marginalia "0.8.0-SNAPSHOT"] ; build docs, 'lein marg'
        [lein-midje "3.1.1"] ; run tests, 'lein midge'
        [cloverage "1.0.3"] ; run tests and check coverage, 'lein cloverage'
        [lein-checkall "0.1.1"] ; check code style, 'lein checkall'
        [perforate "0.3.3"] ; run performance tests, 'lein perforate'
        ]
      :resource-paths ["test_resources"]
      }}
  :main scribble.core)
