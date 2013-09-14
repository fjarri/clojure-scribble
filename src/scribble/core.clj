;; The syntax differs from the Racket's Scribble in the following ways:
;;
;; - `@;` stands for a simple single-line comment,
;;   which just consumes everything until `\newline`.
;; - `@;;` does what `@;` does in Scribble, i.e. consumes everything
;;   until `\newline`, and then until the next non-whitespace character or the second `\newline`.
;; - `{}` reads as a vector of string and nested forms
;;   and is passed as a single argument to the function,
;;   e.g. `@foo{bar @baz{blah}}` reads as `(foo ["bar " (baz ["blah"])])`.
;; - As a result, `@foo{}` reads as `(foo [])` and not as `foo`.
;; - Any number of `[]` and `{}` groups in any order is allowed in the Scribble form
;;   (provided that they are not separated by whitespace),
;;   e.g. `@foo[:bar 2]{baz}[:blah 3]` reads as `(foo :bar 2 ["baz"] :blah 3)`.
;; - Reader macros that follow the `@` are *not* applied to the whole form
;;   that starts with this `@`; rather they are applied to whatever follows
;;   them according to Clojure syntax.
;;   Our reader gets control only when the Clojure reader finishes,
;;   and uses the resulting form as a command.
;;
;; Current problems:
;;
;; - Need to pick a character to use as the entry point,
;;   and how can it still be used according to the standard Clojure syntax
;;   (not critical, providing some rare character is picked, but quite desirable).
;; - Other significant characters (brackets, braces, literal symbol quotes)
;;   may be changed as well.
(ns scribble.core
  (:use [chiara.reader.hacking :only [with-reader-macro]]
        [chiara.reader.macros :only [use-reader-macros]])
  (:require [scribble.reader :refer :all]))

(defn use-scribble
  "Enables the Scribble reader macro in the current namespace."
  []
  (use-reader-macros {:char scribble-char :reader scribble-entry-reader}))

(defmacro with-scribble
  "Temporarily enables the Scribble reader macro."
  [& forms]
  `(with-reader-macro scribble-char scribble-entry-reader (do ~@forms)))
