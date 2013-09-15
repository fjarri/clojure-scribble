;; # Postprocessing for a text block
;;
;; In order to reduce the amount of spurious whitespace of a text block,
;; Scribble truncates leading whitespace and discards
;; trailing whitespace and some newlines, based on certain conditions.
(ns scribble.postprocess
  (:require [scribble.text-accum :refer :all])
  (:import [scribble.text_accum TextToken]))


(defn- whitespace-or-newline?
  [^TextToken token]
  (or
    (.newline? token)
    (.leading-ws? token)
    (.trailing-ws? token)))

(defn- whitespace-only?
  "Returns `true` if the sequence `v` contains only `\\newline``s
  and strings of whitespace characters."
  [^TextToken token]
  (every? whitespace-or-newline? token))

(defn- trim-leading-newline
  "Trim (a possible whitespace string and) a newline string
  from the vector `v`, if they are present."
  [v]
  (if (empty? v)
    v
    (let [^TextToken t-first (nth v 0)]
      (if (= (count v) 1)
        (if (.newline? t-first)
          []
          v)
        (let [^TextToken t-second (nth v 1)]
          (cond
            (.newline? t-first) (subvec v 1)
            (and (.leading-ws? t-first) (.newline? t-second)) (subvec v 2)
            :else v))))))

(defn- trim-trailing-newline
  "Trim a newline string (and a possible whitespace string)
  from the vector `v`, if they are present."
  [v]
  (if (empty? v)
    v
    (let [n-last (dec (count v))
          ^TextToken t-last (nth v n-last)]
      (if (= (count v) 1)
        (if (.newline? t-last)
          []
          v)
        (let [n-prev (dec n-last)
              ^TextToken t-prev (nth v n-prev)]
          (cond
            (.newline? t-last) (subvec v 0 n-last)
            (and (.leading-ws? t-last) (.newline? t-prev)) (subvec v 0 n-prev)
            :else v))))))

(defn- trim-whitespace-pred [indent ^TextToken token]
  (when-not (.trailing-ws? token)
    (if (.leading-ws? token)
      (when (> (count (.contents token)) indent)
        (map-contents #(subs % indent) token))
      token)))

(defn- trim-whitespace [v indent]
  (filterv #(not (nil? %))
    (map (partial trim-whitespace-pred indent) v)))

(defn- find-starting-indent [v]
  ; We need a list of all leading-ws tokens, which are:
  ; - not last,
  ; - have something non-whitespace after them.
  ; Out of those we select the smallest one.
  (let [ws-candidates (subvec v 0 (dec (count v)))
        next-elems (subvec v 1)
        filter-pred (fn [[^TextToken elem ^TextToken next-elem]]
          (and (.leading-ws? elem) (not (.newline? next-elem))))
        ws-pairs (filter filter-pred
          (map (fn [a b] [a b]) ws-candidates next-elems))
        map-pred
          (fn [[^TextToken elem _]] (count (.contents elem)))
        ws-lengths (map map-pred ws-pairs)]
    (if (empty? ws-lengths)
      0
      (reduce min ws-lengths))))

(defn- get-starting-indent [v starting-indent]
  (if (< (count v) 2)
    starting-indent
    (let [t0 ^TextToken (nth v 0)
          t1 ^TextToken (nth v 1)]
      (if (or (.newline? t0) (and (.leading-ws? t0) (.newline? t1)))
        (find-starting-indent v)
        starting-indent))))

(defn- text-merge-starting-whitespace
  "If the text accumulator starts with a leading whitespace
  and a non-whitespace string, merges them together."
  [v]
  (if (< (count v) 2)
    v
    (let [t0 ^TextToken (nth v 0)
          t1 ^TextToken (nth v 1)]
      (if (and (.leading-ws? t0) (string? (.contents t1)) (not (.newline? t1)))
        ; FIXME: prepending is O(N). Need to use finger trees.
        (into [(make-token (str (.contents t0) (.contents t1)))] (subvec v 2))
        v))))

(defn- text-merge-ending-whitespace
  "If the text accumulator ends with a non-whitespace string and
  a trailing whitespace, merges them together."
  [v]
  (if (< (count v) 2)
    v
    (let [n-last (dec (count v))
          n-prev (dec n-last)
          t-last ^TextToken (nth v n-last)
          t-prev ^TextToken (nth v n-prev)]
      (if (and (.trailing-ws? t-last)
               (string? (.contents t-prev))
               (not (.newline? t-prev)))
        (conj
          (subvec v 0 n-prev)
          (make-token (str (.contents t-prev) (.contents t-last))))
        v))))

(defn- text-trim-whitespace
  "Removes excessive whitespace according to the following rules:

  - If the vector starts with a `\\newline`, the size of the
    following whitespace is used instead of `starting-indent`.
  - If the vector only contains `\\newline`s and whitespace,
    everything except `\\newline`s is discarded.
  - Otherwise if it starts with (maybe some whitespace and) `\\newline`,
    or ends with `\\newline` (and maybe some whitespace), these are discarded.
  - Any whitespace right before a `\\newline` is discarded.
  - Any whitespace after a `\\newline` with more than `starting-indent`
    characters is truncated by this amount,
    otherwise it is discarded completely."
  [text-accum starting-indent]
    (let [starting-indent (get-starting-indent text-accum starting-indent)]
      (cond
        (empty? text-accum) text-accum
        (whitespace-only? text-accum)
          (filterv (fn [^TextToken token] (.newline? token)) text-accum)
        :else (-> text-accum
          (trim-whitespace starting-indent)
          trim-leading-newline
          trim-trailing-newline))))

(defn text-postprocess
  [text-accum starting-indent]
  (-> text-accum
    text-merge-starting-whitespace
    text-merge-ending-whitespace
    (text-trim-whitespace starting-indent)
    text-accum-finalize))
