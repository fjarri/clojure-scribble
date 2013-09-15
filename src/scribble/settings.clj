(ns scribble.settings)

(defn whitespace? [c]
  (or (= c \space) (= c \tab)))

(def ^:private clojure-symbol-start-chars
  ; according to http://clojure.org/reader (as of 1.5.1)
  #{\* \+ \! \- \_ \? \/ \: \.})

(defn clojure-symbol-start?
  [^Character c]
  (or
    (Character/isLetter c)
    (contains? clojure-symbol-start-chars c)))

(deftype Settings [
  ^Character entry-char
  ^Character body-start-char
  ^Character body-end-char
  ^Character datum-start-char
  ^Character datum-end-char
  ^Character escape-start-char
  ^Character escape-end-char
  ^Character comment-char
  symbol-end?])

(defn settings
  [entry-char
   body-start-char
   body-end-char
   datum-start-char
   datum-end-char
   escape-start-char
   escape-end-char
   comment-char]
  (let [symbol-end-chars
         (set [entry-char
               body-start-char
               body-end-char
               datum-start-char
               datum-end-char
               escape-start-char
               escape-end-char
               \space
               \tab
               \newline])
        symbol-end? (fn [^Character c] (contains? symbol-end-chars c))]
    (Settings. entry-char
               body-start-char
               body-end-char
               datum-start-char
               datum-end-char
               escape-start-char
               escape-end-char
               comment-char
               symbol-end?)))

(def default-settings (settings \@ \{ \} \[ \] \| \| \;))

(defn entry-char
  [^Settings settings-]
  (.entry-char settings-))

(defn inverse-char
  [c]
  (condp = c
    \( \)
    \) \(
    \[ \]
    \] \[
    \< \>
    \> \<
    c))

(defn inverse-vec
  [v]
  (vec (map inverse-char (reverse v))))
