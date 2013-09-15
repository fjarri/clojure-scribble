(ns scribble.reader
  (:import [clojure.lang LineNumberingPushbackReader])
  (:use [clojure.tools.reader.reader-types :only [reader-error]])
  (:require [chiara.reader.utils :as reader-methods]
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
    (Character/isLetter c)
    (contains? scribble-symbol-start c)))

(def scribble-symbol-end
  #{scribble-char
    scribble-text-start
    scribble-text-end
    scribble-normal-start
    scribble-normal-end
    scribble-verbatim-start
    ;scribble-verbatim-end
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

(defn here-markers
  [here-str]
  (if (nil? here-str)
    [[] []]
    [
      (str scribble-verbatim-start here-str)
      (str
        scribble-text-end
        (clojure.string/join (inverse-vec (vec here-str))))]))

(defn str-pop
  [v n]
  (if (zero? n)
    v
    (subvec v 0 (- (count v) n))))

(declare scribble-entry-reader)

(defn scribble-text-reader
  "Returns a vector of strings and nested forms.
  The strings are separated as
  [leading whitespace, contents, trailing whitespace, newline]
  (for the ease of further processing)."
  [reader here-str]
  (let [[here-start here-end] (here-markers here-str)
        here-marker-len (count here-start)
        escaped (not (nil? here-str))]
  (loop [text-accum []
         str-accum []
         ; FIXME: using a custom type will be faster
         state {:leading-ws true
                :brace-level 0
                :here-str-pos 0}]
    (let [c (reader-methods/read-1 reader)]
      (cond

        (and escaped
             (= c scribble-verbatim-end)
             (= (- (:here-str-pos state)) here-marker-len))
          (if (zero? (:brace-level state))
            (dump-string text-accum (str-pop str-accum here-marker-len))
            (recur text-accum (conj str-accum c)
              (assoc state
                :brace-level (dec (:brace-level state))
                :here-str-pos 0)))

        (and escaped (= c scribble-verbatim-start))
          (let [[text-accum str-accum]
                  (if (:leading-ws state)
                    [(dump-leading-ws text-accum str-accum) [c]]
                    [text-accum (conj str-accum c)])]
              (recur text-accum str-accum
                (assoc state
                  :here-str-pos 1
                  :leading-ws false)))

        (and escaped (= c scribble-text-end))
          (let [[text-accum str-accum]
                  (if (:leading-ws state)
                    [(dump-leading-ws text-accum str-accum) [c]]
                    [text-accum (conj str-accum c)])]
              (recur text-accum str-accum
                (assoc state
                  :here-str-pos -1
                  :leading-ws false)))

        (and escaped
             (= c scribble-text-start)
             (= (:here-str-pos state) here-marker-len))
          (recur text-accum (conj str-accum c)
            (assoc state :here-str-pos 0
                         :brace-level (inc (:brace-level state))))

        (and escaped
             (= c scribble-char)
             (= (:here-str-pos state) here-marker-len))
          (let [nested-form (scribble-entry-reader reader c)
                str-accum (str-pop str-accum here-marker-len)
                [text-accum str-accum]
                  (if (identical? nested-form reader)
                    [text-accum str-accum]
                    (dump-nested-form
                      text-accum str-accum nested-form (:leading-ws state)))]
            (recur text-accum str-accum
              (assoc state :here-str-pos 0)))

        (and escaped
            (pos? (:here-str-pos state))
            (< (:here-str-pos state) here-marker-len)
            (= c (nth here-start (:here-str-pos state))))
          (recur text-accum (conj str-accum c)
            (update-in state [:here-str-pos] inc))

        (and escaped
            (neg? (:here-str-pos state))
            (< (- (:here-str-pos state)) here-marker-len)
            (= c (nth here-end (- (:here-str-pos state)))))
          (recur text-accum (conj str-accum c)
            (update-in state [:here-str-pos] dec))


        ; Starting text-mode symbol
        ; We allow them to appear un-escaped if they are balanced
        (and (not escaped) (= c scribble-text-start) (:leading-ws state))
          (recur (dump-leading-ws text-accum str-accum) [c]
            (assoc state :leading-ws false
                         :brace-level (inc (:brace-level state))))

        (and (not escaped) (= c scribble-text-start))
          (recur text-accum (conj str-accum c)
            (update-in state [:brace-level] inc))

        (and (not escaped)
             (= c scribble-text-end)
             (zero? (:brace-level state)))
          (if (:leading-ws state)
            (dump-leading-ws text-accum str-accum)
            (dump-string text-accum str-accum))
        (and (not escaped) (= c scribble-text-end))
          (if (:leading-ws state)
            (recur (dump-leading-ws text-accum str-accum) [c]
              (assoc state :leading-ws false
                           :brace-level (dec (:brace-level state))))
            (recur text-accum (conj str-accum c)
              (update-in state [:brace-level] dec)))

        (and (not escaped) (= c scribble-char))
          (let [nested-form (scribble-entry-reader reader c)
                [text-accum str-accum]
                  (if (identical? nested-form reader)
                    [text-accum str-accum]
                    (dump-nested-form
                      text-accum str-accum nested-form (:leading-ws state)))]
            (recur text-accum str-accum
              (assoc state :leading-ws false)))



        ; unexpected EOF
        (nil? c)
          (reader-error reader "Unexpected EOF while in text reading mode")

        ; newline encountered: dump accumulator,
        ; turn the leading whitespace mode on
        (= c \newline)
          (let [text-accum
                 (-> text-accum
                   (dump-string str-accum)
                   append-newline)]
            (recur text-accum [] (assoc state :leading-ws true
                                              :here-str-pos 0)))

        ; in leading whitespace mode, whitespace character encountered
        (and (whitespace? c) (:leading-ws state))
          (recur text-accum (conj str-accum c) (assoc state :leading-ws true
                                                            :here-str-pos 0))
        (:leading-ws state)
          (recur (dump-leading-ws text-accum str-accum) [c]
            (assoc state :leading-ws false :here-str-pos 0))

        ; reading characters
        :else (recur text-accum (conj str-accum c)
          (assoc state :here-str-pos 0)))))))

(defn scribble-text-block-reader
  [reader here-str]
  ; FIXME: check that here-str does not contain verbatim start/end chars,
  ; scribble char, or text start/end chars
  (let [[_ c] (reader-methods/reader-position reader)
        column (if (nil? c) 0 c)
        text-accum (scribble-text-reader reader here-str)
        text-form (text-postprocess text-accum column)]
    text-form))

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

(defn scribble-form-reader
  [reader]
  (loop [forms-read []
         ; We want to make a difference between @foo[]
         ; (reads as '(foo)) and @foo (reads as 'foo).
         ; This flag will be set to `true` when
         ; either [] or {} block is encountered.
         forms-present false]
    (let [c (reader-methods/read-1 reader)]
      (cond
        (and (= c scribble-verbatim-start)
             (= (reader-methods/peek reader)
             scribble-text-start))
          (do
            (reader-methods/read-1 reader)
            (recur
              (conj forms-read (scribble-text-block-reader reader ""))
              true))
        (= c scribble-verbatim-start)
          (let [s (read-until reader #(= % scribble-text-start))
                _ (reader-methods/read-1 reader)
                verbatim-form (scribble-text-block-reader reader s)]
            (recur (conj forms-read verbatim-form) true))
        (= c scribble-text-start)
          (recur
            (conj forms-read (scribble-text-block-reader reader nil))
            true)
        (= c scribble-normal-start)
          (let [forms (reader-methods/read-delimited-list
                         scribble-normal-end reader)]
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
        (and newline-encountered (not (whitespace? c)))
          (do (reader-methods/unread reader c) nil)
        (= \newline c) (recur true)
        :else (recur newline-encountered)))))

(defn try-recognize-symbol
  [reader token]
  (if-let [sym (recognize-symbol token)]
    (unwrap-symbol sym)
    (reader-error reader "Invalid symbol: " token)))

(defn read-symbol
  [reader]
  (try-recognize-symbol reader (read-until reader symbol-end?)))

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
      (whitespace? c)
        (reader-error reader
          "Unexpected whitespace at the start of a Scribble form")
      (nil? c)
        (reader-error reader
          "Unexpected EOF at the start of a Scribble form")
      (= c scribble-verbatim-start)
        (let [next-c (reader-methods/peek reader)]
          (if (= next-c scribble-verbatim-start)
            (scribble-form-reader reader)
            (mark-for-splice
              (reader-methods/read-delimited-list
                scribble-verbatim-end reader))))
      :else
        (do
          (reader-methods/unread reader c)
          (let [command (if (symbol-start? c)
                          ; reading a symbol by ourselves,
                          ; because we need to catch '|', which is tecnhically
                          ; allowed in Clojure symbols, and will be consumed by
                          ; `read-next`.
                          (read-symbol reader)
                          (reader-methods/read-next reader))
                forms (scribble-form-reader reader)]
            (cond
              (identical? reader forms) command
              (empty? forms) (list command)
              :else (cons command forms)))))))
