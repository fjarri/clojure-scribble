;; Contains postprocessing functions for the body part,
;; which remove redundant whitespace.
(ns scribble.postprocess
  (:require [scribble.types :refer :all])
  (:import [scribble.types BodyToken]))


(defn- whitespace-or-newline?
  [^BodyToken token]
  (or
    (.newline? token)
    (.leading-ws? token)
    (.trailing-ws? token)))

(defn- whitespace-only?
  "Returns `true` if `text-accum` contains only `\\newline``s
  and strings of whitespace characters."
  [text-accum]
  (every? whitespace-or-newline? text-accum))

(defn- trim-leading-newline
  "Trim (a possible whitespace string and) a newline string
  from `text-accum`, if they are present."
  [text-accum]
  (if (empty? text-accum)
    text-accum
    (let [^BodyToken t-first (nth text-accum 0)]
      (if (= (count text-accum) 1)
        (if (.newline? t-first)
          []
          text-accum)
        (let [^BodyToken t-second (nth text-accum 1)]
          (cond
            (.newline? t-first)
              (subvec text-accum 1)
            (and (.leading-ws? t-first)
                 (.newline? t-second))
              (subvec text-accum 2)
            :else
              text-accum))))))

(defn- trim-trailing-newline
  "Trim a newline string (and a possible whitespace string)
  from `text-accum`, if they are present."
  [text-accum]
  (if (empty? text-accum)
    text-accum
    (let [n-last (dec (count text-accum))
          ^BodyToken t-last (nth text-accum n-last)]
      (if (= (count text-accum) 1)
        (if (.newline? t-last)
          []
          text-accum)
        (let [n-prev (dec n-last)
              ^BodyToken t-prev (nth text-accum n-prev)]
          (cond
            (.newline? t-last)
              (subvec text-accum 0 n-last)
            (and (.leading-ws? t-last)
                 (.newline? t-prev))
              (subvec text-accum 0 n-prev)
            :else
              text-accum))))))

(defn- trim-whitespace-pred
  "The predicate for `trim-whitespace`.
  Checks if the contents of the `token` are leading whitespace
  and trims them by the value of `indent`.
  Returns `nil`, if the leading whitespace is too small,
  these will be cleaned up by `trim-whitespace`."
  [indent ^BodyToken token]
  (when-not (.trailing-ws? token)
    (if (.leading-ws? token)
      (when (> (count (.contents token)) indent)
        (make-body-token (subs (.contents token) indent) :leading-ws :true))
      token)))

(defn- trim-whitespace
  "Trims leading `indent` characters from leading whitespace in `text-accum`"
  [text-accum indent]
  (filterv #(not (nil? %))
    (map (partial trim-whitespace-pred indent) text-accum)))

(defn- zip
  [seq1 seq2]
  (map (fn [a b] [a b]) seq1 seq2))

(defn- find-starting-indent
  "Of all leading whitespace tokens, which are:

   - not last,
   - have something non-whitespace after them,

  we select the smallest one."
  [text-accum]
  (let [ws-candidates (subvec text-accum 0 (dec (count text-accum)))
        next-elems (subvec text-accum 1)

        ; Filter all leading whitespaces followed by something significant
        filter-pred
          (fn [[^BodyToken elem ^BodyToken next-elem]]
            (and (.leading-ws? elem)
                 (not (.newline? next-elem))))
        ws-pairs (filter filter-pred (zip ws-candidates next-elems))

        ; Extract their lengths
        map-pred
          (fn [[^BodyToken elem _]]
            (count (.contents elem)))
        ws-lengths (map map-pred ws-pairs)]
    (if (empty? ws-lengths)
      0
      (reduce min ws-lengths))))

(defn- get-common-indent
  "Returns the common indentation for `text-accum`
  that can be trimmed."
  [text-accum starting-indent]
  (if (< (count text-accum) 2)
    starting-indent
    (let [t0 ^BodyToken (nth text-accum 0)
          t1 ^BodyToken (nth text-accum 1)]
      ; If the body part starts from something significant and not
      ; just whitespace and newline, we take indent of that.
      ; Otherwise we have to search for it in the whole text-accum.
      (if (or (.newline? t0)
              (and (.leading-ws? t0)
                   (.newline? t1)))
        (find-starting-indent text-accum)
        starting-indent))))

(defn- text-merge-starting-whitespace
  "If `text-accum` starts with a leading whitespace
  and a non-whitespace string, merges them together."
  [text-accum]
  (if (< (count text-accum) 2)
    text-accum
    (let [t0 ^BodyToken (nth text-accum 0)
          t1 ^BodyToken (nth text-accum 1)]
      (if (and (.leading-ws? t0)
               (string? (.contents t1))
               (not (.newline? t1)))
        ; FIXME: prepending is O(N).
        ; Need to use something more optimized.
        (into [(make-body-token (str (.contents t0) (.contents t1)))]
              (subvec text-accum 2))
        text-accum))))

(defn- text-merge-ending-whitespace
  "If `text-accum` ends with a non-whitespace string and
  a trailing whitespace, merges them together."
  [text-accum]
  (if (< (count text-accum) 2)
    text-accum
    (let [n-last (dec (count text-accum))
          n-prev (dec n-last)
          t-last ^BodyToken (nth text-accum n-last)
          t-prev ^BodyToken (nth text-accum n-prev)]
      (if (and (.trailing-ws? t-last)
               (string? (.contents t-prev))
               (not (.newline? t-prev)))
        (conj
          (subvec text-accum 0 n-prev)
          (make-body-token (str (.contents t-prev) (.contents t-last))))
        text-accum))))

(defn- text-trim-whitespace
  "Removes excessive whitespace according to the following rules:

  - If the vector starts with a `\\newline`, the size of the
    maximum common whitespace is used instead of `starting-indent`.
  - If the vector only contains `\\newline`s and whitespace,
    everything except `\\newline`s is discarded.
  - Otherwise if it starts with (maybe some whitespace and) `\\newline`,
    or ends with `\\newline` (and maybe some whitespace), these are discarded.
  - Any whitespace right before a `\\newline` is discarded.
  - Any whitespace after a `\\newline` with more than `starting-indent`
    characters is truncated by this amount,
    otherwise it is discarded completely."
  [text-accum starting-indent]
    (let [common-indent (get-common-indent text-accum starting-indent)]
      (cond
        (empty? text-accum) text-accum
        (whitespace-only? text-accum)
          (filterv (fn [^BodyToken token] (.newline? token)) text-accum)
        :else (-> text-accum
          (trim-whitespace common-indent)
          trim-leading-newline
          trim-trailing-newline))))

(defn text-postprocess
  "Postprocess `text-accum` and return the final body part container."
  [text-accum starting-indent]
  (-> text-accum
    text-merge-starting-whitespace
    text-merge-ending-whitespace
    (text-trim-whitespace starting-indent)
    text-accum-finalize))
