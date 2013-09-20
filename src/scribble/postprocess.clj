;; Contains postprocessing functions for the body part,
;; which remove redundant whitespace.
(ns scribble.postprocess
  (:require [scribble.types :refer :all])
  (:import [scribble.types BodyToken]))


(defn- body-part-finalize
  "Converts the body part to the final container
  that is going to be emitted by the reader."
  [body-part]
  (mapv
    (fn [^BodyToken token] (.contents token))
    body-part))


(defn- whitespace-or-newline?
  [^BodyToken token]
  (or
    (.newline? token)
    (.leading-ws? token)
    (.trailing-ws? token)))

(defn- whitespace-only?
  "Returns `true` if `body-part` contains only `\\newline`s
  and strings of whitespace characters."
  [body-part]
  (every? whitespace-or-newline? body-part))

(defn- trim-leading-newline
  "Trim (a possible whitespace string and) a newline string
  from `body-part`, if they are present."
  [body-part]
  (if (empty? body-part)
    body-part
    (let [^BodyToken t-first (nth body-part 0)]
      (if (= (count body-part) 1)
        (if (.newline? t-first)
          []
          body-part)
        (let [^BodyToken t-second (nth body-part 1)]
          (cond
            (.newline? t-first)
              (subvec body-part 1)
            (and (.leading-ws? t-first)
                 (.newline? t-second))
              (subvec body-part 2)
            :else
              body-part))))))

(defn- trim-trailing-newline
  "Trim a newline string (and a possible whitespace string)
  from `body-part`, if they are present."
  [body-part]
  (if (empty? body-part)
    body-part
    (let [n-last (dec (count body-part))
          ^BodyToken t-last (nth body-part n-last)]
      (if (= (count body-part) 1)
        (if (.newline? t-last)
          []
          body-part)
        (let [n-prev (dec n-last)
              ^BodyToken t-prev (nth body-part n-prev)]
          (cond
            (.newline? t-last)
              (subvec body-part 0 n-last)
            (and (.leading-ws? t-last)
                 (.newline? t-prev))
              (subvec body-part 0 n-prev)
            :else
              body-part))))))

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
  "Trims leading `indent` characters from leading whitespace in `body-part`"
  [body-part indent]
  (filterv #(not (nil? %))
    (map (partial trim-leading-whitespace-pred indent) body-part)))

(defn- zip
  [seq1 seq2]
  (map (fn [a b] [a b]) seq1 seq2))

(defn- find-starting-indent
  "Of all leading whitespace tokens, which are:

   - not last,
   - have something non-whitespace after them,

  we select the smallest one."
  [body-part]
  (let [ws-candidates (subvec body-part 0 (dec (count body-part)))
        next-elems (subvec body-part 1)

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
  "Returns the common indentation for `body-part`
  that can be trimmed."
  [body-part starting-indent]
  (if (< (count body-part) 2)
    starting-indent
    (let [t0 ^BodyToken (nth body-part 0)
          t1 ^BodyToken (nth body-part 1)]
      ; If the body part starts from something significant and not
      ; just whitespace and newline, we take indent of that.
      ; Otherwise we have to search for it in the whole body-part.
      (if (or (.newline? t0)
              (and (.leading-ws? t0)
                   (.newline? t1)))
        (find-starting-indent body-part)
        starting-indent))))

(defn- merge-starting-whitespace
  "If `body-part` starts with a leading whitespace
  and a non-whitespace string, merges them together."
  [body-part]
  (if (< (count body-part) 2)
    body-part
    (let [t0 ^BodyToken (nth body-part 0)
          t1 ^BodyToken (nth body-part 1)]
      (if (and (.leading-ws? t0)
               (string? (.contents t1))
               (not (.newline? t1)))
        (assoc (subvec body-part 1)
               0
               (make-body-token (str (.contents t0) (.contents t1))))
        body-part))))

(defn- merge-ending-whitespace
  "If `body-part` ends with a non-whitespace string and
  a trailing whitespace, merges them together."
  [body-part]
  (if (< (count body-part) 2)
    body-part
    (let [n-last (dec (count body-part))
          n-prev (dec n-last)
          t-last ^BodyToken (nth body-part n-last)
          t-prev ^BodyToken (nth body-part n-prev)]
      (if (and (.trailing-ws? t-last)
               (string? (.contents t-prev))
               (not (.newline? t-prev)))
        (conj
          (subvec body-part 0 n-prev)
          (make-body-token (str (.contents t-prev) (.contents t-last))))
        body-part))))

(defn- trim-whitespace
  "Removes excessive whitespace according to the following rules:

  - If the body part starts with a `\\newline`, the size of the
    maximum common whitespace is used instead of `starting-indent`.
  - If the body part only contains `\\newline`s and whitespace,
    everything except `\\newline`s is discarded.
  - Otherwise if it starts with (maybe some whitespace and) `\\newline`,
    or ends with `\\newline` (and maybe some whitespace), these are discarded.
  - Any whitespace right before a `\\newline` is discarded.
  - Any whitespace after a `\\newline` with more than `starting-indent`
    characters is truncated by this amount,
    otherwise it is discarded completely."
  [body-part starting-indent]
    (let [common-indent (get-common-indent body-part starting-indent)]
      (cond
        (empty? body-part) body-part
        (whitespace-only? body-part)
          (filterv (fn [^BodyToken token] (.newline? token)) body-part)
        :else (-> body-part
          (trim-leading-whitespace common-indent)
          trim-leading-newline
          trim-trailing-newline))))

(defn body-postprocess
  "Postprocess `body-accum` and return the final body part container."
  [body-accum starting-indent]
  (-> body-accum
    body-accum-finalize
    merge-starting-whitespace
    merge-ending-whitespace
    (trim-whitespace starting-indent)
    body-part-finalize))
