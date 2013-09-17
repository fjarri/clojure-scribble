;; Containts the main part of the API.
(ns scribble.core
  (:use [chiara.reader.hacking :only [with-reader-macro]]
        [chiara.reader.macros :only [use-reader-macros]])
  (:require [scribble.reader :refer :all]
            [scribble.settings :refer :all]))

(defn use-scribble
  "Enables the Scribble reader macro in the current namespace."
  ([]
    (use-scribble default-settings))
  ([settings]
    (use-reader-macros {:char (entry-char settings)
                        :reader (partial read-entry settings)})))

(defmacro with-scribble-settings
  "Temporarily enables the Scribble reader macro
  with custom settings."
  [settings & exprs]
 `(with-reader-macro
    (entry-char ~settings)
    (partial read-entry ~settings)
    (do ~@exprs)))

(defmacro with-scribble
  "Temporarily enables the Scribble reader macro
  with default settings."
  [& exprs]
 `(with-scribble-settings default-settings ~@exprs))
