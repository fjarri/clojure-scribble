(ns scribble.repr)

(defn repr-char
  "Returns the string representation of the string containing the given character."
  [c]
  (let [ec (char-escape-string c)]
    (if (nil? ec)
      (str c)
      ec)))

(defn repr-str
  "Returns the string representation of the given string."
  [s]
  (clojure.string/join[ "\"" (clojure.string/join (mapv repr-char s)) "\""]))

(defn repr
  "Returns the representation of a form or a string."
  [x]
  (if (string? x)
    (repr-str x)
    (with-out-str (spit *out* x))))
