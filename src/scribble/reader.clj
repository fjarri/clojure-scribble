;; Contains the main reader macro and readers for nested special forms.
(ns scribble.reader
  (:use [clojure.set :only [intersection]])
  (:require [chiara.reader.utils :as reader-methods]
            [scribble.types :refer :all]
            [scribble.postprocess :refer :all]
            [scribble.settings :refer :all])
  (:import [scribble.settings Settings]))


(defn- reader-error
  "Throws an ExceptionInfo with the given message.
  If `reader` provides line/column metadata,
  it will be included in the exception."
  [reader & msg]
  (throw
    (ex-info
      (clojure.string/join msg)
      (merge {:type :reader-exception}
             (when-let [[l c] (reader-methods/reader-position reader)]
               {:line l :column c})))))


(defn- here-markers
  "Creates beginning and ending here-string markers
  for usage in the body part reader."
  [here-str]
  (if (nil? here-str)
    [[] []]
    ; Additional symbols allow us to avoid `dec`s in comparisons.
    [(str \space here-str)
     (str \space (inverse-str here-str))]))

;; The following two functions could be joined into one,
;; but that would require creation and destructuring of a vector
;; `[body-accum str-accum]`, and which noticeably affects
;; `read-body` performance.

(defn- dump-nonws-char-body
  "Updates a body part accumulator
  when a non-whitespace character is encountered"
  [body-accum str-accum leading-ws]
  (if leading-ws
    (dump-leading-ws body-accum str-accum)
    body-accum))

(defn- dump-nonws-char-str
  "Updates a string accumulator
  when a non-whitespace character is encountered"
  [str-accum c leading-ws]
  (if leading-ws
    (make-str-accum c)
    (str-accum-push str-accum c)))

(declare read-entry)

(defn- read-body
  "Returns a vector of strings and nested forms.
  The strings are separated as
  [leading whitespace, contents, trailing whitespace, newline]
  (for the ease of further processing)."
  [^Settings settings reader here-str]
  (let [[here-start here-end] (here-markers here-str)
        here-marker-len (int (count here-start))
        escaped (not (nil? here-str))
        entry-char (.entry-char settings)
        body-start-char (.body-start-char settings)
        body-end-char (.body-end-char settings)
        escape-start-char (.escape-start-char settings)
        escape-end-char (.escape-end-char settings)]
    (loop [body-accum (make-body-accum)
           str-accum (make-str-accum)
           ; Indicates the leading whitespace reading mode.
           leading-ws true
           ; Using privitive integers gets us some speed-up.
           brace-level (int 0)
           ; If the position is positive, we are reading the starting marker,
           ; if it is negative, we are reading the ending marker.
           here-str-pos (int 0)]
      (let [c (reader-methods/read-1 reader)]
        (cond

          ; Catches the body part ending character
          ; (or the escape ending one, if we are in escaped mode)
          (or (and escaped
                   (= c escape-end-char)
                   (== (- here-str-pos) here-marker-len))
              (and (not escaped)
                   (= c body-end-char)))
            (if (zero? brace-level)
              (if leading-ws
                (dump-leading-ws body-accum str-accum)
                (dump-string
                  body-accum
                  (str-accum-pop str-accum here-marker-len)))
              (recur
                (dump-nonws-char-body body-accum str-accum leading-ws)
                (dump-nonws-char-str str-accum c leading-ws)
                false
                (dec brace-level)
                (int 0)))

          ; Catches the body part starting character
          ; (or the escape starting one, if we are in escaped mode)
          (or (and escaped
                   (= c escape-start-char))
              (and (= c body-start-char)
                   (== here-str-pos here-marker-len)))
            (recur
              (dump-nonws-char-body body-accum str-accum leading-ws)
              (dump-nonws-char-str str-accum c leading-ws)
              false
              ; The body-start-char will be at the end of the here-string
              ; sequence in escaped mode, so we can safely increase
              ; the brace level regardless of `escaped`.
              (if (= c body-start-char)
                (inc brace-level)
                brace-level)
              ; If we are in escaped mode and caught the escape starting,
              ; just increase the here-string position.
              (if (= c escape-start-char)
                (int 1)
                (int 0)))

          ; In escaped mode, body-end-char only means that
          ; we should start comparing the following characters with
          ; the ending here-string marker.
          (and escaped
               (= c body-end-char))
            (recur
              (dump-nonws-char-body body-accum str-accum leading-ws)
              (dump-nonws-char-str str-accum c leading-ws)
              false
              brace-level
              (int -1))

          ; Entry character encountered,
          ; and we are at the end of the starting here-string marker.
          (and (= c entry-char)
               (== here-str-pos here-marker-len))
            (let [nested-form (read-entry settings reader c)
                  ; Pop here-string marker from the accumulator
                  str-accum (str-accum-pop str-accum here-marker-len)
                  [body-accum str-accum]
                    ; Check if the nested form was a comment
                    (if (identical? nested-form reader)
                      [body-accum str-accum]
                      (dump-nested-form
                        body-accum str-accum nested-form leading-ws))]
              (recur
                body-accum
                str-accum
                false
                brace-level
                (int 0)))

          ; Reading the starting here-string marker,
          ; and the current character is correct.
          (and (pos? here-str-pos)
               (< here-str-pos here-marker-len)
               (= c (nth here-start here-str-pos)))
            (recur
              body-accum
              (str-accum-push str-accum c)
              leading-ws
              brace-level
              (inc here-str-pos))

          ; Reading the ending here-string marker,
          ; and the current character is correct.
          (and (neg? here-str-pos)
               (< (- here-str-pos) here-marker-len)
               (= c (nth here-end (- here-str-pos))))
            (recur
              body-accum
              (str-accum-push str-accum c)
              leading-ws
              brace-level
              (dec here-str-pos))

          ; Unexpected EOF
          (nil? c)
            (reader-error reader "Unexpected EOF while reading a body part")

          ; Newline encountered: dump accumulator,
          ; turn the leading whitespace mode on
          (= c \newline)
            (let [body-accum
                   (-> body-accum
                     (dump-string str-accum)
                     push-newline)]
              (recur
                body-accum
                (make-str-accum)
                true
                brace-level
                (int 0)))

          ; In leading whitespace mode, a whitespace character encountered
          (and (whitespace? c) leading-ws)
            (recur
              body-accum
              (str-accum-push str-accum c)
              true
              brace-level
              (int 0))

          ; A normal character or a whitespace character
          ; out of the leading whitespace mode
          :else
            (recur
              (dump-nonws-char-body body-accum str-accum leading-ws)
              (dump-nonws-char-str str-accum c leading-ws)
              false
              brace-level
              (int 0)))))))

(defn- validate-here-str
  "Checks that `here-str` does not contain escape-start/end chars,
  entry char, or body-end char (it does not contain the body-start char
  because of the way it was read).
  This makes it easier to watch for it when reading the body part."
  [^Settings settings reader here-str]
  (if (nil? here-str)
    here-str
    (let [prohibited-chars (set [(.entry-char settings)
                                 (.body-end-char settings)
                                 (.escape-start-char settings)
                                 (.escape-end-char settings)])
          here-str-chars (set here-str)]
      (if (empty? (intersection prohibited-chars here-str-chars))
        here-str
        (reader-error reader "Here-string contains invalid characters")))))

(defn- read-body-part
  "Reads a body part, escaped by `here-str`
  (i.e. looking like `` `<here-str>{text here}<inverse-here-str>` ``).
  If `here-str` is `nil`, the body part is considered to be non-escaped."
  [^Settings settings reader here-str]
  (let [here-str (validate-here-str settings reader here-str)
        [_ c] (reader-methods/reader-position reader)
        column (if (nil? c) 0 c)
        body-accum (read-body settings reader here-str)
        body-part (body-postprocess body-accum column)]
    body-part))

(defn- read-until
  "Reads and returns a string until (and not including) `delim`."
  [reader delim]
  (loop [str-accum (make-str-accum)]
    (let [c (reader-methods/read-1 reader)]
      (cond
        (nil? c) (str-accum-finalize str-accum)
        (= delim c)
          (do
            (reader-methods/unread reader c)
            (str-accum-finalize str-accum))
        :else (recur (str-accum-push str-accum c))))))

(defn- read-parts
  "Reads datum and body parts of the form,
  until EOF or whitespace is encountered."
  [^Settings settings reader]
  (let [body-start-char (.body-start-char settings)
        datum-start-char (.datum-start-char settings)
        datum-end-char (.datum-end-char settings)
        escape-start-char (.escape-start-char settings)]
    (loop [forms-read []
           ; We want to make a difference between `@foo[]`
           ; (reads as `'(foo)`) and `@foo` (reads as `'foo`).
           ; This flag will be set to `true` when
           ; either datum or body part is encountered.
           forms-present false]
      (let [c (reader-methods/read-1 reader)]
        (cond

          ; Simple escaped string, e.g. the one starting with just
          ; escape start char + body start char
          (and (= c escape-start-char)
               (= (reader-methods/peek reader) body-start-char))
            (do
              (reader-methods/read-1 reader)
              (recur
                (conj forms-read (read-body-part settings reader ""))
                true))

          ; An escaped string with non-empty here-string
          (= c escape-start-char)
            (let [here-str (read-until reader body-start-char)
                  next-c (reader-methods/read-1 reader)]
              (if (nil? next-c)
                (reader-error reader
                  "Unexpected EOF while reading a here-string")
                (let [body-part (read-body-part settings reader here-str)]
                  (recur
                    (conj forms-read body-part)
                    true))))

          ; A simple body part
          (= c body-start-char)
            (recur
              (conj forms-read (read-body-part settings reader nil))
              true)

          ; A datum part
          (= c datum-start-char)
            (let [forms (reader-methods/read-delimited-list
                           datum-end-char reader)]
              (recur (vec (concat forms-read forms)) true))

          (nil? c)
            (if forms-present
              (list* forms-read)
              reader)

          :else
            (do
              (reader-methods/unread reader c)
              (if forms-present
                (list* forms-read)
                reader)))))))

(defn- skip-to-newline
  "Reads from `reader` until `\\newline` or `EOF` is encountered
  (the final `\\newline` is not consumed).
  Returns `nil`."
  [reader]
  (loop []
    (let [c (reader-methods/read-1 reader)]
      (cond
        (nil? c) nil
        (= \newline c) (do (reader-methods/unread reader c) nil)
        :else (recur)))))

(defn- skip-to-meaningful-char
  "Reads from `reader` until `\\newline` is encountered
  and then until the first non-whitespace character is encountered
  (the final character is not consumed), or until `EOF` is encountered.
  Returns `nil`."
  [reader]
  (loop [newline-encountered false]
    (let [c (reader-methods/read-1 reader)]
      (cond
        (nil? c) nil
        (and newline-encountered (not (whitespace? c)))
          (do (reader-methods/unread reader c) nil)
        (= \newline c) (recur true)
        :else (recur newline-encountered)))))

(defn read-entry
  "The entry point of the reader macro."
  [^Settings settings reader _]
  (let [body-start-char (.body-start-char settings)
        datum-start-char (.datum-start-char settings)
        escape-start-char (.escape-start-char settings)
        escape-end-char (.escape-end-char settings)
        comment-char (.comment-char settings)
        c (reader-methods/read-1 reader)]
    (cond

      ; No command, body or datum starts right away.
      ; Pass the execution to the parts reader.
      (or (= c body-start-char)
          (= c datum-start-char))
        (do
          (reader-methods/unread reader c)
          (read-parts settings reader))

      ; A comment form
      (= c comment-char)
        (let [next-c (reader-methods/peek reader)]
          (condp = next-c
            comment-char (skip-to-meaningful-char reader)
            body-start-char (read-parts settings reader)
            (skip-to-newline reader))
          ; By convention, if the reader function has read nothing,
          ; it returns the reader.
          reader)

      (whitespace? c)
        (reader-error reader
          "Unexpected whitespace at the start of a Scribble form")

      (nil? c)
        (reader-error reader
          "Unexpected EOF at the start of a Scribble form")

      ; An escaped (spliced) form, or an escaped string.
      (= c escape-start-char)
        (let [next-c (reader-methods/peek reader)]
          (if (= next-c escape-start-char)
            (read-parts settings reader)
            ; If it is a spliced form, mark it with metadata,
            ; so that it could be spliced in the parent reader.
            (mark-for-splice
              (reader-methods/read-delimited-list
                escape-end-char reader))))

      :else
        (do
          (reader-methods/unread reader c)
          (let [command (reader-methods/read-next reader)
                forms (read-parts settings reader)]
            (cond
              (identical? reader forms) command
              (empty? forms) (list command)
              :else (cons command forms)))))))
