(ns scribble.core-test
  (:require [clojure.test :refer :all]
            [scribble.utils :refer :all]
            [scribble.core :refer :all]))


; Unfortunately, the reader that reads from strings does not have line/column metadata.
; So in order for Scribble whitespace truncation to work properly,
; we need to use the main reader.
(use-scribble)


; Tests for the reader macro

(def forms [

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

  ['@foo{blah blah
         yada yada}
   '(foo ["blah blah" "\n"
      "yada yada"])]

  ['@foo{
      blah blah
      yada yada
   }
   '(foo [
      "blah blah" "\n"
      "yada yada"])]

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

  ['@foo{bar @; comment
         baz@;
         blah}
   '(foo ["bar bazblah"])]

  ])

(deftest test-reading
  (doseq [[scribble-form expected-form] forms]
    (testing (repr expected-form)
      (is (= scribble-form expected-form)))))
