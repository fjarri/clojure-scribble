;; This namespace contains some copy-paste of symbol-recognition code
;; from `clojure.tools.reader`.
;; Unfortunately, it is private and coupled to `reader`
;; (which makes it difficult to work with escaped symbols),
;; that's why we are not importing it.
(ns scribble.symbol
  (:use [clojure.tools.reader.impl.commons :only [parse-symbol]]))

;; A simple wrapper for a symbol returned by `recognize-symbol`,
;; because that symbol can be nil, and we can't even wrap it in a list.
(deftype WrappedSymbol [sym])

(defn unwrap-symbol
  [^WrappedSymbol wrapped-sym]
  (.sym wrapped-sym))

; (modified `read-symbol` from clojure.tools/reader)
(defn recognize-symbol
  "Checks if the string has the proper format for a symbol,
  and returns the `'(symbol)` (to account for the symbol being `nil`),
  or `nil` if the recognition failed."
  [^String token]
  (case token
    ; special symbols
    "nil" (WrappedSymbol. nil)
    "true" (WrappedSymbol. true)
    "false" (WrappedSymbol. false)
    "/" (WrappedSymbol. '/)
    "NaN" (WrappedSymbol. Double/NaN)
    "-Infinity" (WrappedSymbol. Double/NEGATIVE_INFINITY)
    ("Infinity" "+Infinity") (WrappedSymbol. Double/POSITIVE_INFINITY)
    (when-let [p (parse-symbol token)]
      (WrappedSymbol. (symbol (p 0) (p 1))))))
