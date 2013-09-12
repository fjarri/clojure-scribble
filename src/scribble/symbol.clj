;; This namespace contains some copy-paste of symbol-recognition code
;; from `clojure.tools.reader`.
;; Unfortunately, it is private and coupled to `reader`
;; (which makes it difficult to work with escaped symbols),
;; that's why we are not importing it.
(ns scribble.symbol
  (:use [clojure.tools.reader.impl.commons :only [parse-symbol]]))

; (modified `read-symbol` from clojure.tools/reader)
(defn recognize-symbol
  "Checks if the string has the proper format for a symbol,
  and returns the `(symbol)` (to account for the symbol being `nil`),
  or `nil` if the recognition failed."
  [^String token]
  (case token
    ; special symbols
    "nil" (list nil)
    "true" (list true)
    "false" (list false)
    "/" (list '/)
    "NaN" (list Double/NaN)
    "-Infinity" (list Double/NEGATIVE_INFINITY)
    ("Infinity" "+Infinity") (list Double/POSITIVE_INFINITY)
    (when-let [p (parse-symbol token)]
      (list (symbol (p 0) (p 1))))))
