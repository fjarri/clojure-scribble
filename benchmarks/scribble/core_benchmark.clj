(ns scribble.core-benchmark
  (:require [clojure.java.io :as io])
  (:use perforate.core)
  (:use [chiara.reader.hacking :only [set-reader-macro]])
  (:use scribble.core)
  (:use scribble.settings))

; Making `|` character macro-terminating,
; so that the test file could be read properly.
; FIXME: changes the macro table globally!
; Should be replaced by something safer, e.g.:
; - with-reader-macros (when it's available from chiara),
; - or if we start to check for non-terminating characters
;   on settings creation and set stub readers for them ourselves
;   (somewhere in with-scribble-settings maybe).
(defn- reader-stub [reader _] reader)
(set-reader-macro \| reader-stub)

(defgoal reader-bench "Reader benchmark.")

(defcase reader-bench :scribble-doc-load
  []
  (with-scribble-settings
    (make-settings \@ \{ \} \[ \] \| \| \;)
      (load-file (.getPath (io/resource "doc.txt")))))
