(ns scribble.core
  (:use [clarity.reader.hacking :only [with-reader-macro]])
  (:use [clarity.reader.macros :only [use-reader-macros]])
  (:require [clarity.reader.utils :as reader-utils])
  (:import [clojure.lang Util LispReader LineNumberingPushbackReader])
  (:require [scribble.utils :refer :all]))


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

(defn discard-whitespace [v n]
  (println "-- discard whitespace" (count v) n (< (dec n) (count v)))
  (if (< (dec n) (count v))
    (subvec v n)
    []))

(declare scribble-entry-reader)

(defn dump-nested-form [vec-accum str-accum nested-form]
  (if (nil? nested-form)
    ; it was a comment
    [vec-accum str-accum]
    ; an actual form
    (let [new-vec-accum (dump-accum vec-accum str-accum)]
      [(conj new-vec-accum nested-form) []])))

; Returns a vector of strings and nested forms.
; The strings are separated as [leading whitespace, contents, trailing whitespace, newline]
; (for the ease of further processing).
(defn scribble-text-reader [reader column]
  (println "- In scribble-text-reader")
  (loop [vec-accum []
         str-accum []
         leading-ws false
         newline-encountered false]
    (let [c (my-read-1 reader)]
      (cond
        ; end of text mode
        (= c scribble-text-end) (dump-accum vec-accum str-accum)
        ; start of a Scribble form
        (= c scribble-char) (let [nested-form (scribble-entry-reader reader c)
                                  [vec-accum str-accum] (dump-nested-form vec-accum str-accum nested-form)]
          (println "-- nested form" (repr nested-form))
          (recur vec-accum str-accum leading-ws newline-encountered))
        ; unexpected EOF
        (nil? c) (throw (ex-info "Unexpected EOF while in text reading mode"))
        ; newline encountered: dump accumulator, turn leading whitespace mode on
        (and leading-ws (= c \newline)) (recur (conj vec-accum "\n") [] true true)
        (= c \newline) (recur (conj (dump-accum vec-accum str-accum true) "\n") [] true true)
        ; in leading whitespace mode, whitespace character encountered
        (and leading-ws (whitespace? c)) (recur vec-accum (conj str-accum c) true newline-encountered)
        ; In leading whitespace mode, non-whitespace character encountered;
        ; need to discard `column` characters of the leading whitespace
        ; if there has been at least one newline in this text form.
        (and leading-ws newline-encountered (not (nil? column)))
          (recur (dump-accum vec-accum (discard-whitespace str-accum column)) [c] false newline-encountered)
        (true? leading-ws) (recur (dump-accum vec-accum str-accum) [c] false newline-encountered)
        ; reading characters
        :else (recur vec-accum (conj str-accum c) false newline-encountered)))))

(defn scribble-normal-reader [reader]
  (println "- In scribble-normal-reader")
  (let [forms (my-read-delimited-list scribble-normal-end reader)
        _ (my-read-1 reader)]
    forms))

(defn scribble-form-reader [reader forms-read]
  (println "- In scribble-form-reader, forms read:" (repr (vec forms-read)))
  (let [c (my-read-1 reader)]
    (condp = c
      scribble-text-start (let [[_ column] (reader-position reader)
                                form (scribble-text-reader reader column)
                                forms-read (concat forms-read (list form))]
        (scribble-form-reader reader forms-read))
      scribble-normal-start (let [forms (my-read-delimited-list scribble-normal-end reader)
                                  forms-read (concat forms-read forms)]
        (scribble-form-reader reader forms-read))
      nil forms-read
      (do (reader-utils/unread reader c) forms-read))))

(defn skip-to-next-line [reader]
  (loop []
    (let [c (reader-utils/read-1 reader)]
      (if-not (= \newline c)
        (recur)))))

(defn scribble-entry-reader [reader _]
  (println "- In scribble-entry-reader")
  (let [c (my-peek reader)]
    (condp = c
      scribble-comment (do (skip-to-next-line reader) (reader-utils/unread reader \newline) nil)
      (let [sym (my-read-next reader)
            forms (scribble-form-reader reader ())]
        (println "-- entry-reader finished " (repr (cons sym forms)))
        (cons sym forms)))))

(defn use-scribble []
  (use-reader-macros {:char scribble-char :reader scribble-entry-reader}))

(defmacro with-scribble [& forms]
  `(with-reader-macro scribble-char scribble-entry-reader (do ~@forms)))
