(ns scribble.core-test
  (:require [clojure.test :refer :all]
            [scribble.utils :refer :all]
            [scribble.core :refer :all]))


; Tests for the reader macro

(defn read-scribble [to-read]
  (let [form (with-scribble (read-string to-read))]
    (println "*** Read:" (repr form))
    form))

(defn read-as [to-read expected-form]
  (is (= (read-scribble to-read) expected-form)))

(def forms [

  ; The Scribble Syntax at a Glance

  ["@foo{blah blah blah}"
  '(foo ["blah blah blah"])]
  ["@foo{blah \"blah\" (`blah'?)}"
  '(foo ["blah \"blah\" (`blah'?)"])]
  ["@foo[1 2]{3 4}"
  '(foo 1 2 ["3 4"])]
  ["@foo[1 2 3 4]"
  '(foo 1 2 3 4)]
  ["@foo[:width 2]{blah blah}"
  '(foo :width 2 ["blah blah"])]

  [
"@foo{blah blah
     yada yada}"
'(foo ["blah blah" "\n"
  "     " "yada yada"])]

  [
"@foo{
  blah blah
  yada yada
}"
'(foo ["\n"
"  " "blah blah" "\n"
"  " "yada yada" "\n"
])]

  [
"@foo{bar @baz{3}
     blah}"
'(foo ["bar " (baz ["3"]) "\n"
  "     " "blah"])]

  [
"@foo{@b{@u[3] @u{4}}
     blah}"
'(foo [(b [(u 3) " " (u ["4"])]) "\n"
  "     " "blah"])]

  [
"@C{while (*(p++))
   *p = '\\n';}"
'(C ["while (*(p++))" "\n"
  "   " "*p = '\\n';"])]

  [
"@foo{bar @; comment
      baz@;
      blah}"
'(foo "bar bazblah")]

  ])

(deftest test-reading
  (doseq [[to-read expected-form] forms]
    (testing (repr to-read)
      (read-as to-read expected-form))))
