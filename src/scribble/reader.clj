(ns scribble.reader
  (:import [clojure.lang LineNumberingPushbackReader])
  (:use [clojure.tools.reader.reader-types :only [reader-error]])
  (:require [clarity.reader.utils :as reader-methods]
            [scribble.text-accum :refer :all]
            [scribble.postprocess :refer :all]
            [scribble.symbol :refer :all]))


(defn whitespace? [c]
  (or (= c \space) (= c \tab)))

(def scribble-char \@)
(def scribble-text-start \{)
(def scribble-text-end \})
(def scribble-normal-start \[)
(def scribble-normal-end \])
(def scribble-verbatim-start \|)
(def scribble-verbatim-end \|)
(def scribble-comment \;)


(def scribble-symbol-start
  ; according to http://clojure.org/reader (as of 1.5.1)
  #{\* \+ \! \- \_ \?})

(defn symbol-start?
  [^Character c]
  (or
    (Character/isLetterOrDigit c)
    (contains? scribble-symbol-start c)))

(def scribble-symbol-end
  #{scribble-char
    scribble-text-start
    scribble-normal-start
    scribble-verbatim-start
    \space
    \tab
    \newline})

(defn symbol-end?
  [c]
  (contains? scribble-symbol-end c))

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


(defn reader-position [^LineNumberingPushbackReader reader]
  (if (instance? LineNumberingPushbackReader reader)
    [(-> reader .getLineNumber int) (-> reader .getColumnNumber dec int)]))


(declare scribble-entry-reader)

(defn scribble-text-reader
  "Returns a vector of strings and nested forms.
  The strings are separated as [leading whitespace, contents, trailing whitespace, newline]
  (for the ease of further processing)."
  [reader escaped]
  (loop [text-accum []
         str-accum []
         ; FIXME: using a custom type will be faster
         state {:leading-ws true
                :brace-level 0
                :escaped-scribble-char false}]
    (let [c (reader-methods/read-1 reader)]
      (cond

        ; Starting text-mode symbol
        ; We allow them to appear un-escaped if they are balanced
        (and (not escaped) (= c scribble-text-start) (:leading-ws state))
          (recur (dump-leading-ws text-accum str-accum) [c]
            (assoc state :leading-ws false :brace-level (inc (:brace-level state))))

        (and (not escaped) (= c scribble-text-start))
          (recur text-accum (conj str-accum c)
            (update-in state [:brace-level] inc))

        ; end of text mode
        (or
          (and (not escaped) (zero? (:brace-level state)) (= c scribble-text-end))
          (and escaped (= c scribble-text-end) (= (reader-methods/peek reader) scribble-verbatim-end)))
          (if (:leading-ws state)
            (dump-leading-ws text-accum str-accum)
            (dump-string text-accum str-accum :separate-trailing-ws true))

        (and (not escaped) (= c scribble-text-end) (:leading-ws state))
          (recur (dump-leading-ws text-accum str-accum) [c]
            (assoc state :leading-ws false :brace-level (dec (:brace-level state))))
        (and (not escaped) (= c scribble-text-end))
          (recur text-accum (conj str-accum c)
            (update-in state [:brace-level] dec))

        ; start of a Scribble form
        (and (= c scribble-verbatim-start) escaped)
          (if (= (reader-methods/peek reader) scribble-char)
            (recur text-accum str-accum (assoc state :escaped-scribble-char true))
            (recur text-accum (conj str-accum c) state))
        (and (= c scribble-char) (or (not escaped) (:escaped-scribble-char state)))
          (let [nested-form (scribble-entry-reader reader c)
                [text-accum str-accum]
                  (if (identical? nested-form reader)
                    [text-accum str-accum]
                    (dump-nested-form text-accum str-accum nested-form))]
            (recur text-accum str-accum
              (assoc state :leading-ws false :escaped-scribble-char false)))

        ; unexpected EOF
        (nil? c) (reader-error reader "Unexpected EOF while in text reading mode")

        ; newline encountered: dump accumulator, turn leading whitespace mode on
        (= c \newline)
          (let [text-accum
                 (-> text-accum
                   (dump-string str-accum :separate-trailing-ws true)
                   append-newline)]
            (recur text-accum [] (assoc state :leading-ws true)))

        ; in leading whitespace mode, whitespace character encountered
        (and (whitespace? c) (:leading-ws state))
          (recur text-accum (conj str-accum c) (assoc state :leading-ws true))
        (:leading-ws state)
          (recur (dump-leading-ws text-accum str-accum) [c] (assoc state :leading-ws false))

        ; reading characters
        :else (recur text-accum (conj str-accum c) state)))))

(defn scribble-text-block-reader
  [reader escaped]
  (let [[_ c] (reader-position reader)
        column (if (nil? c) 0 c)
        text-accum (scribble-text-reader reader escaped)
        text-form (text-postprocess text-accum column)]
    (if escaped
      (let [c (reader-methods/read-1 reader)]
        (if (= c scribble-verbatim-end)
          text-form
          (reader-error reader "Did not find the matching end of an escaped text block")))
      text-form)))

(defn read-until
  [reader stop-condition?]
  (loop [chars []]
    (let [c (reader-methods/read-1 reader)]
      (cond
        (nil? c) (clojure.string/join chars)
        (stop-condition? c)
          (do
            (reader-methods/unread reader c)
            (clojure.string/join chars))
        :else (recur (conj chars c))))))

(defn read-until-vec
  [reader end-vec]
  (let [end-vec-len (count end-vec)]
    (loop [buffer []]
      (let [c (reader-methods/read-1 reader)
            buffer (conj buffer c)
            buffer-len (count buffer)
            rest-len (- buffer-len end-vec-len)]
        (if (and (> buffer-len end-vec-len) (= end-vec (subvec buffer rest-len)))
          (clojure.string/join (subvec buffer 0 rest-len))
          (recur buffer))))))

(defn scribble-verbatim-read
  [reader here-str]
  (let [end-vec (concat [scribble-text-end] (inverse-vec (vec here-str)) [scribble-verbatim-end])]
    (reader-methods/read-1 reader) ; read `scribble-text-start`
    [(read-until-vec reader end-vec)]))

(defn scribble-verbatim-reader
  [reader]
  (let [here-vec (read-until reader #(= % scribble-text-start))]
    (scribble-verbatim-read reader (clojure.string/join here-vec))))

(defn scribble-form-reader
  [reader]
  (loop [forms-read []
         ; We want to make a difference between @foo[]
         ; (reads as '(foo)) and @foo (reads as 'foo).
         ; This flag will be set to `true` when either [] or {} block is encountered.
         forms-present false]
    (let [c (reader-methods/read-1 reader)]
      (cond
        (and (= c scribble-verbatim-start) (= (reader-methods/peek reader) scribble-text-start))
          (do
            (reader-methods/read-1 reader)
            (recur (conj forms-read (scribble-text-block-reader reader true)) true))
        (= c scribble-verbatim-start)
          (recur (conj forms-read (scribble-verbatim-reader reader)) true)
        (= c scribble-text-start)
          (recur (conj forms-read (scribble-text-block-reader reader false)) true)
        (= c scribble-normal-start)
          (let [forms (reader-methods/read-delimited-list scribble-normal-end reader)]
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
              reader))))))

(defn skip-to-newline
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

(defn skip-to-meaningful-char
  "Reads from `reader` until `\\newline` is encountered
  and then until the first non-whitespace character is encountered
  (the final character is not consumed), or until `EOF` is encountered.
  Returns `nil`."
  [reader]
  (loop [newline-encountered false]
    (let [c (reader-methods/read-1 reader)]
      (cond
        (and newline-encountered (not (whitespace? c))) (do (reader-methods/unread reader c) nil)
        (= \newline c) (recur true)
        :else (recur newline-encountered)))))

(defn try-recognize-symbol
  [reader token]
  (if-let [sym (recognize-symbol token)]
    (unwrap-symbol sym)
    (reader-error reader "Invalid symbol: " token)))

(defn read-symbol
  [reader verbatim]
  (if verbatim
    (let [sym (try-recognize-symbol reader
                (read-until reader #(or (= % scribble-verbatim-end) (= % scribble-text-start))))]
      (reader-methods/read-1 reader)
      sym)
    (try-recognize-symbol reader (read-until reader symbol-end?))))

(defn scribble-entry-reader
  "The entry point of the reader macro."
  [reader _]
  (let [c (reader-methods/read-1 reader)]
    (cond
      (or (= c scribble-text-start) (= c scribble-normal-start))
        (do
          (reader-methods/unread reader c)
          (scribble-form-reader reader))
      (= c scribble-comment)
        (let [next-c (reader-methods/peek reader)]
          (condp = next-c
            scribble-comment (skip-to-meaningful-char reader)
            scribble-text-start (scribble-form-reader reader)
            (skip-to-newline reader))
          ; By convention, if the reader function has read nothing,
          ; it returns the reader.
          reader)
      (whitespace? c) (reader-error reader "Unexpected whitespace at the start of a Scribble form")
      (nil? c) (reader-error reader "Unexpected EOF at the start of a Scribble form")
      (= c scribble-verbatim-start)
        (let [s (read-until reader #(or (= % scribble-verbatim-end) (= % scribble-text-start)))
              c (reader-methods/peek reader)]
          (if (= c scribble-verbatim-end)
            (do (reader-methods/read-1 reader) (try-recognize-symbol reader s))
            (let [verbatim-form (scribble-verbatim-read reader s)
                  forms (scribble-form-reader reader)]
              (cond
                (identical? reader forms) (list verbatim-form)
                (empty? forms) (list verbatim-form)
                :else (cons verbatim-form forms)))))
      :else
        (do
          (reader-methods/unread reader c)
          (let [command (if (symbol-start? c)
                          ; reading a symbol by ourselves,
                          ; because we need to catch '|', which is tecnhically
                          ; allowed in Clojure symbols, and will be consumed by
                          ; `read-next`.
                          (read-symbol reader false)
                          (reader-methods/read-next reader))
                forms (scribble-form-reader reader)]
            (cond
              (identical? reader forms) command
              (empty? forms) (list command)
              :else (cons command forms)))))))
