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

(declare scribble-entry-reader)

; Returns a vector of strings and nested forms.
; The strings are separated as [leading whitespace, contents, trailing whitespace, newline]
; (for the ease of further processing).
(defn scribble-text-reader [reader]
  (println "- In scribble-text-reader")
  (loop [vec-accum []
         str-accum []
         leading-ws false]
    (let [c (my-read-1 reader)]
      (cond
        ; end of text mode
        (= c scribble-text-end) (dump-accum vec-accum str-accum)
        ; start of a Scribble form
        (= c scribble-char) (let [new-vec-accum (dump-accum vec-accum str-accum)
                                  nested-form (scribble-entry-reader reader c)]
          (println "-- nested form" (repr nested-form))
          (recur (conj new-vec-accum nested-form) [] true))
        ; unexpected EOF
        (nil? c) (throw (ex-info "Unexpected EOF while in text reading mode"))
        ; newline encountered: dump accumulator, turn leading whitespace mode on
        (= c \newline) (recur (conj (dump-accum vec-accum str-accum (not leading-ws)) "\n") [] true)
        ; in leading whitespace mode, whitespace character encountered
        (and leading-ws (whitespace? c)) (recur vec-accum (conj str-accum c) true)
        ; in leading whitespace mode, non-whitespace character encountered
        (true? leading-ws) (recur (dump-accum vec-accum str-accum) [c] false)
        ; reading characters
        :else (recur vec-accum (conj str-accum c) false)))))


; Gets a vector with text parts (with \n's as separate pieces) and nested forms,
; and the column of the starting character of the text.
; Performs the following tasks:
; 1)
(defn postprocess-text [raw-text column]
  raw-text)

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
                                raw-text (scribble-text-reader reader)
                                form (postprocess-text raw-text column)
                                forms-read (concat forms-read (list form))]
        (scribble-form-reader reader forms-read))
      scribble-normal-start (let [forms (my-read-delimited-list scribble-normal-end reader)
                                  forms-read (concat forms-read forms)]
        (scribble-form-reader reader forms-read))
      nil forms-read
      (do (reader-utils/unread reader c) forms-read))))

(defn scribble-entry-reader [reader _]
  (println "- In scribble-entry-reader")
  (let [c (my-peek reader)]
    (condp = c
      (let [sym (my-read-next reader)
            forms (scribble-form-reader reader ())]
        (cons sym forms)))))

(defmacro with-scribble [& forms]
  `(with-reader-macro scribble-char scribble-entry-reader (do ~@forms)))
