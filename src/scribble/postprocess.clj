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
  "Returns `true` if `body-accum` contains only `\\newline``s
  and strings of whitespace characters."
  [body-accum]
  (every? whitespace-or-newline? body-accum))

(defn- trim-leading-newline
  "Trim (a possible whitespace string and) a newline string
  from `body-accum`, if they are present."
  [body-accum]
  (if (empty? body-accum)
    body-accum
    (let [^BodyToken t-first (nth body-accum 0)]
      (if (= (count body-accum) 1)
        (if (.newline? t-first)
          []
          body-accum)
        (let [^BodyToken t-second (nth body-accum 1)]
          (cond
            (.newline? t-first)
              (subvec body-accum 1)
            (and (.leading-ws? t-first)
                 (.newline? t-second))
              (subvec body-accum 2)
            :else
              body-accum))))))

(defn- trim-trailing-newline
  "Trim a newline string (and a possible whitespace string)
  from `body-accum`, if they are present."
  [body-accum]
  (if (empty? body-accum)
    body-accum
    (let [n-last (dec (count body-accum))
          ^BodyToken t-last (nth body-accum n-last)]
      (if (= (count body-accum) 1)
        (if (.newline? t-last)
          []
          body-accum)
        (let [n-prev (dec n-last)
              ^BodyToken t-prev (nth body-accum n-prev)]
          (cond
            (.newline? t-last)
              (subvec body-accum 0 n-last)
            (and (.leading-ws? t-last)
                 (.newline? t-prev))
              (subvec body-accum 0 n-prev)
            :else
              body-accum))))))

(defn- trim-leading-whitespace-pred
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

(defn- trim-leading-whitespace
  "Trims leading `indent` characters from leading whitespace in `body-accum`"
  [body-accum indent]
  (filterv #(not (nil? %))
    (map (partial trim-leading-whitespace-pred indent) body-accum)))

(defn- zip
  [seq1 seq2]
  (map (fn [a b] [a b]) seq1 seq2))

(defn- find-starting-indent
  "Of all leading whitespace tokens, which are:

   - not last,
   - have something non-whitespace after them,

  we select the smallest one."
  [body-accum]
  (let [ws-candidates (subvec body-accum 0 (dec (count body-accum)))
        next-elems (subvec body-accum 1)

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
  "Returns the common indentation for `body-accum`
  that can be trimmed."
  [body-accum starting-indent]
  (if (< (count body-accum) 2)
    starting-indent
    (let [t0 ^BodyToken (nth body-accum 0)
          t1 ^BodyToken (nth body-accum 1)]
      ; If the body part starts from something significant and not
      ; just whitespace and newline, we take indent of that.
      ; Otherwise we have to search for it in the whole body-accum.
      (if (or (.newline? t0)
              (and (.leading-ws? t0)
                   (.newline? t1)))
        (find-starting-indent body-accum)
        starting-indent))))

(defn- merge-starting-whitespace
  "If `body-accum` starts with a leading whitespace
  and a non-whitespace string, merges them together."
  [body-accum]
  (if (< (count body-accum) 2)
    body-accum
    (let [t0 ^BodyToken (nth body-accum 0)
          t1 ^BodyToken (nth body-accum 1)]
      (if (and (.leading-ws? t0)
               (string? (.contents t1))
               (not (.newline? t1)))
        ; FIXME: prepending is O(N).
        ; Need to use something more optimized.
        (into [(make-body-token (str (.contents t0) (.contents t1)))]
              (subvec body-accum 2))
        body-accum))))

(defn- merge-ending-whitespace
  "If `body-accum` ends with a non-whitespace string and
  a trailing whitespace, merges them together."
  [body-accum]
  (if (< (count body-accum) 2)
    body-accum
    (let [n-last (dec (count body-accum))
          n-prev (dec n-last)
          t-last ^BodyToken (nth body-accum n-last)
          t-prev ^BodyToken (nth body-accum n-prev)]
      (if (and (.trailing-ws? t-last)
               (string? (.contents t-prev))
               (not (.newline? t-prev)))
        (conj
          (subvec body-accum 0 n-prev)
          (make-body-token (str (.contents t-prev) (.contents t-last))))
        body-accum))))

(defn- trim-whitespace
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
  [body-accum starting-indent]
    (let [common-indent (get-common-indent body-accum starting-indent)]
      (cond
        (empty? body-accum) body-accum
        (whitespace-only? body-accum)
          (filterv (fn [^BodyToken token] (.newline? token)) body-accum)
        :else (-> body-accum
          (trim-leading-whitespace common-indent)
          trim-leading-newline
          trim-trailing-newline))))

(defn body-postprocess
  "Postprocess `body-accum` and return the final body part container."
  [body-accum starting-indent]
  (-> body-accum
    merge-starting-whitespace
    merge-ending-whitespace
    (trim-whitespace starting-indent)
    body-accum-finalize))
