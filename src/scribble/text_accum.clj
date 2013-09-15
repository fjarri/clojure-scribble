(ns scribble.text-accum)


(defn- str-accum-finalize [str-accum]
  (clojure.string/join str-accum))

(defn- str-accum-finalize-trimr [str-accum]
  (let [s (str-accum-finalize str-accum)
        trimmed-s (clojure.string/trimr s)
        count-full (count s)
        count-trimmed (count trimmed-s)]
    (if (= count-full count-trimmed)
      [s ""]
      [trimmed-s (subs s count-trimmed)])))


(deftype TextToken [
  contents
  ^Boolean newline?
  ^Boolean leading-ws?
  ^Boolean trailing-ws?])

(defn make-token
  [contents & {:keys [newline leading-ws trailing-ws]
               :or {newline false
                    leading-ws false
                    trailing-ws false}}]
  (TextToken. contents newline leading-ws trailing-ws))

(defn map-contents [f ^TextToken token]
  (TextToken.
    (f (.contents token))
    (.newline? token)
    (.leading-ws? token)
    (.trailing-ws? token)))


(defn- text-accum-append
  [text-accum s]
  (conj text-accum s))

(defn text-accum-finalize
  [text-accum]
  (mapv
    (fn [^TextToken token] (.contents token))
    text-accum))


(defn- append-trailing-ws
  [text-accum s]
  (if (empty? s)
    text-accum
    (text-accum-append text-accum (make-token s :trailing-ws true))))

(defn- append-string
  [text-accum s]
  (if (empty? s)
    text-accum
    (text-accum-append text-accum (make-token s))))

(defn- append-form
  [text-accum f]
  (text-accum-append text-accum (make-token f)))

(defn append-newline
  [text-accum]
  (text-accum-append text-accum (make-token "\n" :newline true)))

(defn dump-leading-ws
  [text-accum str-accum]
  (text-accum-append
    text-accum
    (make-token (str-accum-finalize str-accum) :leading-ws true)))

(defn- dump-string-verbatim
  [text-accum str-accum]
  (append-string text-accum (str-accum-finalize str-accum)))

(defn dump-string
  "Joins `str-accum` in a string and attaches it to the end of
  `text-accum`, returning the resulting vector.
  If `str-accum` is empty, `vec-accum` is returned unchanged.
  If `separate-trailing-ws` is `true`, the string constructed from `str-accum`
  is split into the main part and the trailing whitespace part
  before the attachment to `vec-accum`."
  [text-accum str-accum]
  (let [[s trailing-ws] (str-accum-finalize-trimr str-accum)]
    (-> text-accum
      (append-string s)
      (append-trailing-ws trailing-ws))))

(defn mark-for-splice
  [l]
  (with-meta l {::splice true}))

(defn dump-nested-form
  "Need to return `str-accum` because in case of the comment
  we do not want to break the string."
  [text-accum str-accum nested-form leading-ws]
  (cond
    leading-ws
      (dump-nested-form
        (dump-leading-ws text-accum str-accum) [] nested-form false)

    ; it was a string: special case, append it to the accumulator
    (string? nested-form)
      ; FIXME: prepending is O(n)
      [text-accum (vec (concat str-accum nested-form))]

    ; an actual form
    :else
      (let [text-accum-with-str (dump-string-verbatim text-accum str-accum)
            text-accum
              (if (::splice (meta nested-form))
                (reduce append-form text-accum-with-str nested-form)
                (append-form text-accum-with-str nested-form))]
        [text-accum []])))
