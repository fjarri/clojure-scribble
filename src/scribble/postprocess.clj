;; # Postprocessing for a text block
;;
;; In order to reduce the amount of spurious whitespace of a text block,
;; Scribble truncates leading whitespace and discards
;; trailing whitespace and some newlines, based on certain conditions.
;;
;; Theoretically, this can be done during reading, but performing it separately
;; makes the code more readable.

(ns scribble.postprocess
  (:require [scribble.text-accum :refer :all])
  (:import [scribble.text_accum TextToken]))

(defn id
  "Used to help the coverage tester."
  [x]
  x)

(defn whitespace-or-newline?
  [^TextToken token]
  (or
    (.newline? token)
    (.leading-ws? token)
    (.trailing-ws? token)))

(defn whitespace-only?
  "Returns `true` if the sequence `v` contains only `\\newline``s
  and strings of whitespace characters."
  [^TextToken token]
  (every? whitespace-or-newline? token))

(defn trim-leading-newline
  "Trim (a possible whitespace string and) a newline string from the vector `v`,
  if they are present."
  [v]
  (if (empty? v)
    (id v)
    (let [^TextToken t-first (nth v 0)]
      (if (= (count v) 1)
        (if (.newline? t-first)
          (id [])
          (id v))
        (let [^TextToken t-second (nth v 1)]
          (cond
            (.newline? t-first) (subvec v 1)
            (and (.leading-ws? t-first) (.newline? t-second)) (subvec v 2)
            :else (id v)))))))

(defn trim-trailing-newline
  "Trim a newline string (and a possible whitespace string) from the vector `v`,
  if they are present."
  [v]
  (if (empty? v)
    (id v)
    (let [n-last (dec (count v))
          ^TextToken t-last (nth v n-last)]
      (if (= (count v) 1)
        (if (.newline? t-last)
          (id [])
          (id v))
        (let [n-prev (dec n-last)
              ^TextToken t-prev (nth v n-prev)]
          (cond
            (.newline? t-last) (subvec v 0 n-last)
            (and (.leading-ws? t-last) (.newline? t-prev)) (subvec v 0 n-prev)
            :else (id v)))))))

(defn trim-whitespace-pred [indent ^TextToken token]
  (when-not (.trailing-ws? token)
    (if (.leading-ws? token)
      (when (> (count (.contents token)) indent)
        (map-contents #(subs % indent) token))
      token)))

(defn trim-whitespace [v indent]
  (filterv #(not (nil? %))
    (map (partial trim-whitespace-pred indent) v)))


(defn get-starting-indent [v starting-indent]
  (if (> (count v) 1)
    (let [t0 ^TextToken (nth v 0)
          t1 ^TextToken (nth v 1)]
      (if (and (.newline? t0) (.leading-ws? t1))
        (-> t1 .contents count)
        starting-indent))
    starting-indent))

(defn text-trim-whitespace
  "Removes excessive whitespace according to the following rules:

  - If the vector starts with a `\\newline`, the size of the following whitespace
    is used instead of `starting-indent`.
  - If the vector only contains `\\newline`s and whitespace,
    everything except `\\newline`s is discarded.
  - Otherwise if it starts with (maybe some whitespace and) `\\newline`,
    or ends with `\\newline` (and maybe some whitespace), these are discarded.
  - Any whitespace right before a `\\newline` is discarded.
  - Any whitespace after a `\\newline` with more than `starting-indent` characters
    is truncated by this amount, otherwise it is discarded completely."
  [text-accum starting-indent]
    (let [starting-indent (get-starting-indent text-accum starting-indent)]
      (cond
        (empty? text-accum) text-accum
        (whitespace-only? text-accum) (filterv (fn [^TextToken token] (.newline? token)) text-accum)
        :else (-> text-accum
          (trim-whitespace starting-indent)
          trim-leading-newline
          trim-trailing-newline))))

(defn text-postprocess
  [text-accum starting-indent]
  (-> text-accum
    (text-trim-whitespace starting-indent)
    text-accum-finalize))
