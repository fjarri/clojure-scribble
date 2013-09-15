;; This namespace contains some copy-paste of symbol-recognition code
;; from `clojure.tools.reader`.
;; Unfortunately, it is private and coupled to `reader`
;; (which makes it difficult to work with escaped symbols),
;; that's why we are not importing it.
(ns scribble.symbol)

;; A simple wrapper for a symbol returned by `recognize-symbol`,
;; because that symbol can be nil, and we can't even wrap it in a list.
(deftype WrappedSymbol [sym])

(defn unwrap-symbol
  [^WrappedSymbol wrapped-sym]
  (.sym wrapped-sym))

(defn parse-symbol
  "Copy of `parse-symbol` from `clojure.tools.reader.impl.commons`,
  with `numeric?` replaced by `.isDigit`."
  [^String token]
  (when-not (or (identical? "" token)
                (not= -1 (.indexOf token "::")))
    (let [ns-idx (.indexOf token "/")]
      (if-let [ns (and (pos? ns-idx)
                       (subs token 0 ns-idx))]
        (let [ns-idx (inc ns-idx)]
          (when-not (== ns-idx (count token))
            (let [sym (subs token ns-idx)]
              (when (and (not (Character/isDigit (.charAt sym 0)))
                         (not (identical? "" sym))
                         (or (= sym "/")
                             (== -1 (.indexOf sym "/"))))
                [ns sym]))))
        (when (or (= token "/")
                  (== -1 (.indexOf token "/")))
            [nil token])))))

(defn recognize-symbol
  "Modified `read-symbol` from `clojure.tools.reader.impl.commons`.

  Checks if the string has the proper format for a symbol,
  and returns the `'(symbol)` (to account for the symbol being `nil`),
  or `nil` if the recognition failed."
  [^String token]
  (case token
    ; special symbols
    "nil" (WrappedSymbol. nil)
    "true" (WrappedSymbol. true)
    "false" (WrappedSymbol. false)
    "/" (WrappedSymbol. '/)

    ; CLJ-1074 is not merged yet, so these literals do not work
    ;"NaN" (WrappedSymbol. Double/NaN)
    ;"-Infinity" (WrappedSymbol. Double/NEGATIVE_INFINITY)
    ;("Infinity" "+Infinity") (WrappedSymbol. Double/POSITIVE_INFINITY)

    (when-let [p (parse-symbol token)]
      (WrappedSymbol. (symbol (p 0) (p 1))))))
