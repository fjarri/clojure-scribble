(ns scribble.utils)


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


(defn whitespace? [c]
  (or (= c \space) (= c \tab)))


(defn find-last
  "Finds the position of the last occurence of an element of an iterable `v`
  for which the application of `predicate` returns `true`.
  If no such elements are found, returns `nil`.
  The iterable must support `nth`, preferably with O(1) complexity."
  [predicate v]
  (when (seq v)
    (loop [pos (dec (count v))]
      (let [elem (nth v pos)]
        (cond
          (predicate elem) pos
          (zero? pos) nil
          :else (recur (dec pos)))))))


(defn dump-accum
  "Joins `str-accum` (a vector of characters) in a string and attaches it to the end of
  `vec-accum` (a vector of strings), returning the resulting vector.
  If `str-accum` is empty, `vec-accum` is returned unchanged.
  If `separate-trailing-ws` is `true`, the string constructed from `str-accum` is split into
  main part and trailing whitespace part before the attachment to `vec-accum`."
  ([vec-accum str-accum]
    (if (empty? str-accum)
      vec-accum
      (conj vec-accum (clojure.string/join str-accum))))
  ([vec-accum str-accum separate-trailing-ws]
    (if-not separate-trailing-ws
      (dump-accum vec-accum str-accum)
      (let [last-non-ws (find-last #(not (whitespace? %)) str-accum)]
        (if (nil? last-non-ws)
          (dump-accum vec-accum str-accum)
          (let [main-part (subvec str-accum 0 (inc last-non-ws))
                trailing-ws (subvec str-accum (inc last-non-ws))]
            (-> vec-accum (dump-accum main-part) (dump-accum trailing-ws))))))))
