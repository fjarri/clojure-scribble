;; Contains methods for the string accumulator, the body part accumulator,
;; and the body part token.
;; This way the underlying data structures can be changed easily if needed,
;; and the complexity requirements can be assessed.
(ns scribble.types)


;; # String accumulator methods
;;
;; String accumulator is used when the reader reads and collects characters
;; from the stream.

(defn make-str-accum
  "Creates an empty string accumulator."
  ([]
    [])
  ([c]
    [c]))

(defn str-accum-pop
  "Removes the last `n` characters from `str-accum`."
  [str-accum n]
  (if (zero? n)
    str-accum
    (subvec str-accum 0 (- (count str-accum) n))))

(defn str-accum-push
  "Adds a character `c` to the end of `str-accum`."
  [str-accum c]
  (conj str-accum c))

(defn str-accum-finalize
  "Returns a string representing the contents of the accumulator."
  [str-accum]
  (clojure.string/join str-accum))


;; # Body part token methods
;;
;; When the body part is read, it is organized into tokens
;; containing strings or arbitrary forms, and some metadata,
;; which is readily available at read time,
;; but would require `O(n)` time to obtain during the postprocessing.

(deftype BodyToken [
  contents
  ; `true` if `contents` is a string "\n"
  ^boolean newline?
  ; `true` if `contents` is a string of whitespace characters,
  ; representing the leading whitespace in the body part.
  ^boolean leading-ws?
  ; `true` if `contents` is a string of whitespace characters,
  ; representing the trailing whitespace in the body part.
  ^boolean trailing-ws?])

(defn make-body-token
  "Creates a `BodyToken` with an optional metadata."
  [contents & {:keys [newline leading-ws trailing-ws]
               :or {newline false
                    leading-ws false
                    trailing-ws false}}]
  (BodyToken. contents
              (boolean newline)
              (boolean leading-ws)
              (boolean trailing-ws)))



(defn- text-accum-append
  [text-accum s]
  (conj text-accum s))

(defn text-accum-finalize
  [text-accum]
  (mapv
    (fn [^BodyToken token] (.contents token))
    text-accum))


(defn- append-trailing-ws
  [text-accum s]
  (if (empty? s)
    text-accum
    (text-accum-append text-accum (make-body-token s :trailing-ws true))))

(defn- append-string
  [text-accum s]
  (if (empty? s)
    text-accum
    (text-accum-append text-accum (make-body-token s))))

(defn- append-form
  [text-accum f]
  (text-accum-append text-accum (make-body-token f)))

(defn append-newline
  [text-accum]
  (text-accum-append text-accum (make-body-token "\n" :newline true)))

(defn dump-leading-ws
  [text-accum str-accum]
  (text-accum-append
    text-accum
    (make-body-token (str-accum-finalize str-accum) :leading-ws true)))

(defn- dump-string-verbatim
  [text-accum str-accum]
  (append-string text-accum (str-accum-finalize str-accum)))

(defn- split-trimr
  "Splits the string into two strings containing the trailing whitespace
  and the remaining part.
  Returns a vector `[main-part trailing-ws]`."
  [s]
  (let [trimmed-s (clojure.string/trimr s)
        count-full (count s)
        count-trimmed (count trimmed-s)]
    (if (= count-full count-trimmed)
      [s ""]
      [trimmed-s (subs s count-trimmed)])))

(defn dump-string
  "Joins `str-accum` in a string and attaches it to the end of
  `text-accum`, returning the resulting vector.
  If `str-accum` is empty, `vec-accum` is returned unchanged.
  If `separate-trailing-ws` is `true`, the string constructed from `str-accum`
  is split into the main part and the trailing whitespace part
  before the attachment to `vec-accum`."
  [text-accum str-accum]
  (let [[main-part trailing-ws] (split-trimr (str-accum-finalize str-accum))]
    (-> text-accum
      (append-string main-part)
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
