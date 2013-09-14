(ns scribble.core-benchmark
  (:require [clojure.java.io :as io])
  (:use perforate.core)
  (:use scribble.core))

(defgoal reader-bench "Reader benchmark.")

(defcase reader-bench :scribble-doc-load
  []
  (with-scribble
    (load-file (.getPath (io/resource "doc.scrbl")))))
