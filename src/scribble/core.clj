;; The syntax differs from the Racket's Scribble in the following ways:
;;
;; - `@;` stands for a simple single-line comment,
;;   which just consumes everything until `\newline`.
;; - `@;;` does what `@;` does in Scribble, i.e. consumes everything
;;   until `\newline`, and then until the next non-whitespace character or the second `\newline`.
;; - `{}` reads as a vector of string and nested forms
;;   and is passed as a single argument to the function,
;;   e.g. `@foo{bar @baz{blah}}` reads as `(foo ["bar " (baz ["blah"])])`.
;; - Any number of `[]` and `{}` groups in any order is allowed in the Scribble form
;;   (provided that they are not separated by whitespace),
;;   e.g. `@foo[:bar 2]{baz}[:blah 3]` reads as `(foo :bar 2 ["baz"] :blah 3)`.

(ns scribble.core
  (:use [clarity.reader.hacking :only [with-reader-macro]])
  (:use [clarity.reader.macros :only [use-reader-macros]])
  (:require [clarity.reader.utils :as reader-utils])
  (:import [clojure.lang Util LispReader LineNumberingPushbackReader])
  (:require [scribble.accumulators :refer :all])
  (:require [scribble.postprocess :refer :all])
  (:require [scribble.repr :refer :all]))


(def scribble-char \@)
(def scribble-text-start \{)
(def scribble-text-end \})
(def scribble-normal-start \[)
(def scribble-normal-end \])
(def scribble-symbol-start \|)
(def scribble-symbol-end \|)
(def scribble-comment \;)


(defn reader-position [reader]
  (if (instance? clojure.lang.LineNumberingPushbackReader reader)
    [(-> reader .getLineNumber int) (-> reader .getColumnNumber dec int)]))


(defn my-peek [reader]
  (let [c (reader-utils/peek reader)]
    (do
      (println "* peek:" c)
      c)))

(defn my-read-1 [reader]
  (let [c (reader-utils/read-1 reader)]
    (do
      (println "* read-1:" c)
      c)))

(defn my-read-next [reader]
  (println "* reading next starting from" (reader-utils/peek reader))
  (let [form (reader-utils/read-next reader)]
    (do
      (println "* read-next:" (repr form))
      form)))

(defn my-read-delimited-list [delim reader]
  (println "* reading delimited list starting from" (reader-utils/peek reader))
  (let [form (reader-utils/read-delimited-list delim reader)]
    (do
      (println "* read-delimited-list:" (repr form))
      form)))

(defn reader-error [reader message]
  (let [[l c] (reader-position reader)]
    (ex-info message {:line l :column c})))

(defn whitespace? [c]
  (or (= c \space) (= c \tab)))

(declare scribble-entry-reader)

(defn scribble-text-reader
  "Returns a vector of strings and nested forms.
  The strings are separated as [leading whitespace, contents, trailing whitespace, newline]
  (for the ease of further processing)."
  [reader]
  (loop [text-accum []
         str-accum []
         leading-ws true]
    (let [c (my-read-1 reader)]
      (cond

        ; end of text mode
        (= c scribble-text-end)
          (if leading-ws
            (dump-leading-ws text-accum str-accum)
            (dump-string text-accum str-accum))

        ; start of a Scribble form
        (= c scribble-char)
          (let [nested-form (scribble-entry-reader reader c)
                [text-accum str-accum]
                  (dump-nested-form text-accum str-accum nested-form)]
            (recur text-accum str-accum false))

        ; unexpected EOF
        (nil? c) (throw (reader-error reader "Unexpected EOF while in text reading mode"))

        ; newline encountered: dump accumulator, turn leading whitespace mode on
        (= c \newline)
          (let [text-accum
                 (-> text-accum
                   (dump-string str-accum :separate-trailing-ws true)
                   append-newline)]
            (recur text-accum [] true))

        ; in leading whitespace mode, whitespace character encountered
        (and leading-ws (whitespace? c)) (recur text-accum (conj str-accum c) true)
        (true? leading-ws) (recur (dump-leading-ws text-accum str-accum) [c] false)

        ; reading characters
        :else (recur text-accum (conj str-accum c) false)))))

(defn scribble-normal-reader [reader]
  (println "- In scribble-normal-reader")
  (let [forms (my-read-delimited-list scribble-normal-end reader)
        _ (my-read-1 reader)]
    forms))

(defn scribble-form-reader [reader forms-read]
  (println "- In scribble-form-reader, forms read:" (repr (vec forms-read)))
  (let [c (my-read-1 reader)]
    (condp = c
      scribble-text-start
        (let [[_ column] (reader-position reader)
              text-accum (scribble-text-reader reader)
              text-form (text-postprocess text-accum column)
              forms-read (concat forms-read (list text-form))]
          (scribble-form-reader reader forms-read))
      scribble-normal-start
        (let [forms (my-read-delimited-list scribble-normal-end reader)
              forms-read (concat forms-read forms)]
          (scribble-form-reader reader forms-read))
      nil forms-read
      (do (reader-utils/unread reader c) forms-read))))

(defn skip-to-newline
  "Reads from `reader` until `\\newline` or `EOF` is encountered
  (the final `\\newline` is not consumed).
  Returns `nil`."
  [reader]
  (loop []
    (let [c (my-read-1 reader)]
      (cond
        (nil? c) nil
        (= \newline c) (do (reader-utils/unread reader c) nil)
        :else (recur)))))

(defn skip-to-meaningful-char
  "Reads from `reader` until `\\newline` is encountered
  and then until the first non-whitespace character is encountered
  (the final character is not consumed), or until `EOF` is encountered.
  Returns `nil`."
  [reader]
  (loop [newline-encountered false]
    (let [c (my-read-1 reader)]
      (cond
        (and newline-encountered (not (whitespace? c))) (do (reader-utils/unread reader c) nil)
        (= \newline c) (recur true)
        :else (recur newline-encountered)))))

(defn scribble-entry-reader
  "The entry point of the reader macro."
  [reader _]
  (println "- In scribble-entry-reader")
  (let [c (my-read-1 reader)]
    (condp = c
      scribble-comment (let [next-c (my-peek reader)]
        (if (= next-c scribble-comment)
          (skip-to-meaningful-char reader)
          (skip-to-newline reader)))
      nil (throw (reader-error "Unexpected EOF at the start of a Scribble form"))
      (do
        (reader-utils/unread reader c)
        (let [sym (my-read-next reader)
              forms (scribble-form-reader reader ())]
          (println "-- entry-reader finished " (repr (cons sym forms)))
          (cons sym forms))))))

(defn use-scribble
  "Enables the Scribble reader macro in the current namespace."
  []
  (use-reader-macros {:char scribble-char :reader scribble-entry-reader}))

(defmacro with-scribble
  "Temporarily enables the Scribble reader macro."
  [& forms]
  `(with-reader-macro scribble-char scribble-entry-reader (do ~@forms)))
