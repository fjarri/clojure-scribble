(ns scribble.core-test
  (:require [clojure.test :refer :all]
            [scribble.repr :refer :all]
            [scribble.core :refer :all]))


; Unfortunately, the reader that reads from strings does not have line/column metadata.
; So in order for Scribble whitespace truncation to work properly,
; we need to use the main reader.
(use-scribble)


; Tests for the reader macro
; Mostly taken from http://docs.racket-lang.org/scribble/reader.html

(def forms [

  ; difference from the original Scribble syntax: @; is a normal comment,
  ; @;; is a TeX-like (whitespace-consuming) comment

  ; this also tests that the whitespace right before the newline
  ; (see the first line) is discarded.
  ['@foo{bar @; comment
         baz@;
         blah}
   '(foo ["bar" "\n"
      "baz" "\n"
      "blah"])]

  ['@foo{bar @;; comment
         baz@;;
         blah}
   '(foo ["bar bazblah"])]

  ; The Scribble Syntax at a Glance

  ['@foo{blah blah blah}
   '(foo ["blah blah blah"])]
  ['@foo{blah "blah" (`blah'?)}
   '(foo ["blah \"blah\" (`blah'?)"])]
  ['@foo[1 2]{3 4}
   '(foo 1 2 ["3 4"])]
  ['@foo[1 2 3 4]
   '(foo 1 2 3 4)]
  ['@foo[:width 2]{blah blah}
   '(foo :width 2 ["blah blah"])]

  ; If the beginning { is followed by something other than \n, all indentation is counted from it.
  ; If the indentation of the line is bigger, the base indentation is subtracted from it.
  ; If it is smaller, it is discarder completely.
  ; NOTE: currently 1 \tab = 1 \space. Clojure's reader counts \tab as one symbol anyway.
  ['@foo{blah blah
         yada yada
           ddd
       ttt}
   '(foo ["blah blah" "\n"
      "yada yada" "\n"
      "  " "ddd" "\n"
      "ttt"])]

  ; If the beginning { is directly followed by \n,
  ; The starting indentation is taken from the next line.
  ; Same rules as before apply for the remaining lines.
  ['@foo{
      blah blah
      yada yada
        ddd
    ttt
   }
   '(foo [
      "blah blah" "\n"
      "yada yada" "\n"
      "  " "ddd" "\n"
      "ttt"])]

  ; Leading newlines trimming

  ['@foo{bar @baz{3}
         blah}
   '(foo ["bar " (baz ["3"]) "\n"
      "blah"])]

  ['@foo{@b{@u[3] @u{4}}
         blah}
   '(foo [(b [(u 3) " " (u ["4"])]) "\n"
      "blah"])]

  ['@C{while (*(p++))
       *p = '\n';}
   '(C ["while (*(p++))" "\n"
      "*p = '\\n';"])]

  ; Missing command part

  ['@{blah blah}
   '(["blah blah"])]
  ['@{blah @[3]}
   '(["blah " (3)])]
  ['@{foo
      bar
      baz}
   '(["foo" "\n"
      "bar" "\n"
      "baz"])]

  ; Command part only

  ['@foo
   'foo]
  ['@{blah @foo blah}
   '(["blah " foo " blah"])]
  ['@{blah @foo- blah} ; ':' in identifiers has special meaning in Clojure, so changed it to '-'
   '(["blah " foo- " blah"])]
  ['@{blah @|foo|- blah}
   '(["blah " foo "- blah"])]
  ; after a '||'-delimited symbol the text mode starts right away
  ['@{blah @|foo|[3] blah}
   '(["blah " foo "[3] blah"])]

  ])

(deftest test-reading
  (doseq [[scribble-form expected-form] forms]
    (testing (repr expected-form)
      (is (= scribble-form expected-form)))))
