(ns scribble.reader-test
  (:import [java.lang RuntimeException])
  (:use [midje.sweet])
  (:require [clojure.test :refer :all]
            [scribble.core :refer :all]
            [scribble.settings :refer :all]))


; Tests were taken from the orignal Scribble documentation,
; so we use its symbols.
; (except for the \` instead of \|,
; because the latter is not macro-terminating)
(def scribble-settings (make-settings \@ \{ \} \[ \] \` \` \;))

; Unfortunately, the reader that reads from strings does not have
; line/column metadata.
; So in order for the whitespace truncation to work properly,
; we need to use the main reader.
(use-scribble scribble-settings)


; For exception tests, and cases where EOF at a certain place is needed.
(defn read-scribble [s]
  (with-scribble scribble-settings (read-string s)))


; Tests for the reader macro
; Mostly taken from http://docs.racket-lang.org/scribble/reader.html

(deftest test-reader-syntax (facts "about the syntax"

  ; difference from the original Scribble syntax: @; is a normal comment,
  ; @;; is a TeX-like (whitespace-consuming) comment
  (fact "a whitespace between a line comment and a newline is discarded"
   '@foo{bar @; comment
         baz@;
         blah}
    =>
   '(foo ["bar" "\n"
      "baz" "\n"
      "blah"]))

  (fact "a consuming comment joins lines"
   '@foo{bar @;; comment
         baz@;;
         blah}
    =>
   '(foo ["bar bazblah"]))

  ; The Scribble Syntax at a Glance

  (fact "a simple line"
   '@foo{blah blah blah}
    =>
   '(foo ["blah blah blah"]))

  (fact "quotes in a body part"
   '@foo{blah "blah" (`blah'?)}
    =>
   '(foo ["blah \"blah\" (`blah'?)"]))

  (fact "a normal block and a body part"
   '@foo[1 2]{3 4}
    =>
   '(foo 1 2 ["3 4"]))

  (fact "a single normal block"
   '@foo[1 2 3 4]
    =>
   '(foo 1 2 3 4))

  (fact "a non-trivial syntax in a normal block"
   '@foo[:width 2]{blah blah}
    =>
   '(foo :width 2 ["blah blah"]))

  ; If the beginning { is followed by something other than \n,
  ; all indentation is counted from it.
  ; If the indentation of the line is bigger, the base indentation
  ; is subtracted from it.
  ; If it is smaller, it is discarder completely.
  ; NOTE: currently 1 \tab = 1 \space. Clojure's reader counts
  ; \tab as one symbol anyway.
  (fact "leading indentation is truncated"
   '@foo{blah blah
         yada yada
           ddd
       ttt}
    =>
   '(foo ["blah blah" "\n"
      "yada yada" "\n"
      "  " "ddd" "\n"
      "ttt"]))

  (fact "leading indentation is truncated in front of nested forms"
   '@foo{blah blah
         @yada{yada}
           @ddd{ttt}}
    =>
   '(foo ["blah blah" "\n"
      (yada ["yada"]) "\n"
      "  " (ddd ["ttt"])]))

  ; If the beginning { is directly followed by \n,
  ; the starting indentation is taken from the leftmost non-empty line.
  (fact "leading indentation and the starting newline are truncated"
   '@foo{
      blah blah
      yada yada
        ddd
  @; non-consuming comment
    ttt
   }
    =>
   '(foo [
      "  " "blah blah" "\n"
      "  " "yada yada" "\n"
      "    " "ddd" "\n"
      "\n"
      "ttt"]))

  (fact "leading indentation with nested forms is truncated"
   '@foo{bar @baz{3}
         blah}
    =>
   '(foo ["bar " (baz ["3"]) "\n"
      "blah"]))

  (fact "leading indentation with nested forms in the beginning is truncated"
   '@foo{@b{@u[3] @u{4}}
         blah}
    =>
   '(foo [(b [(u 3) " " (u ["4"])]) "\n"
      "blah"]))

  (fact "whitespace is attached to the text at the ends of a body part"
   '@foo{ aa
         b
         c }
    =>
   '(foo [" aa" "\n" "b" "\n" "c "]))


  ; Missing command part

  (fact "missing command part"
   '@{blah blah}
    =>
   '(["blah blah"]))

  (fact "missing command part with a nested form"
   '@{blah @[3]}
    =>
   '(["blah " (3)]))

  (fact "missing command part with multiline text"
   '@{foo
      bar
      baz}
    =>
   '(["foo" "\n"
      "bar" "\n"
      "baz"]))

  ; Command part only

  (fact "command part only"
   '@foo
    =>
   'foo)

  (fact "command part only in a body part"
   '@{blah @foo blah}
    =>
   '(["blah " foo " blah"]))

  ; ':' in identifiers has special meaning in Clojure, so changed it to '-'
  (fact "non-trivial command in a body part"
   '@{blah @foo- blah}
    =>
   '(["blah " foo- " blah"]))

  (fact "escaped command in a body part"
   '@{blah @`foo`- blah}
    =>
   '(["blah " foo "- blah"]))

  (fact "body part resumes right after an escaped command"
   '@{blah @`foo`[3] blah}
    =>
   '(["blah " foo "[3] blah"]))

  (fact "arbitrary form as a command"
   '@foo{(+ 1 2) -> @(+ 1 2)!}
   =>
   '(foo ["(+ 1 2) -> " (+ 1 2) "!"]))

  (fact "a command-like string is attached to the surrounding text"
   '@foo{A @"string" escape}
    =>
   '(foo ["A string escape"]))

  (fact "the entry character wrapped in a string"
   '@foo{eli@"@"barzilay.org}
    =>
   '(foo ["eli@barzilay.org"]))

  (fact "the body part delimiter wrapped in a string"
   '@foo{A @"{" begins a block}
    =>
   '(foo ["A { begins a block"]))

  (fact "balanced body part delimiters do not require escaping"
   '@C{while (*(p++)) {
         *p = '\n';
      }}
    =>
   '(C ["while (*(p++)) {" "\n"
       "  " "*p = '\\n';" "\n"
       "}"]))

  ; A regression for a bug in the body part reader logic
  (fact "body part delimiters at the beginning of a body part work correctly"
   '@foo{{{}}{}}
    =>
   '(foo ["{{}}{}"]))

  ; Here strings

  (fact "unbalanced body part delimiters in an escaped body part"
   '@foo`{bar}@{baz}`
   =>
   '(foo ["bar}@{baz"]))

  (fact "balanced escaped body part delimiters in an escaped body part"
   '@foo`{Nesting `{is}` ok}`
    =>
   '(foo ["Nesting `{is}` ok"]))

  (fact "an escaped nested command in an escaped body part"
   '@foo`{bar `@x{X} baz}`
    =>
   '(foo ["bar " (x ["X"]) " baz"]))

  (fact "an escaped nested body part in an escaped body part"
   '@foo`{bar `@x`{@}` baz}`
    =>
   '(foo ["bar " (x ["@"]) " baz"]))

  ; check that there is no off-by-one error because the delimiter is
  ; two symbols instead of one
  (fact "an escaped body part truncates leading indentation properly"
   '@foo`{Maze
          `@bar{is}
         Life!
           blah blah}`
    =>
   '(foo ["Maze" "\n"
      (bar ["is"]) "\n"
      "Life!" "\n"
      " " "blah blah"]))

  (fact "an escaped body part with delimiters"
   '@foo`--{bar}@`{baz}--`
    =>
   '(foo ["bar}@`{baz"]))

  (fact "an escaped body part with mirrored delimiters"
   '@foo`<(-[{bar}@`{baz}]-)>`
    =>
   '(foo ["bar}@`{baz"]))

  (fact "an escaped body part with a nested form"
   '@foo`!!{X `!!@b{Y}...}!!`
    =>
   '(foo ["X " (b ["Y"]) "..."]))

  ; Empty blocks

  (fact "an empty normal block is ignored"
   '@foo[]{bar} => '(foo ["bar"]))
  (fact "an empty normal block and a missing body part result in a form"
   '@foo[] => '(foo))
  (fact "a single command is read as a symbol"
   '@foo => 'foo)
  (fact "an empty body part results in an empty text container"
   '@foo{} => '(foo []))


  ; The Command Part

  ; Difference from the original Scribble!
  ; Since I do not feel like parsing all the custom reader macros in existence,
  ; everything after @ that does not look like a symbol gets passed
  ; to the Clojure reader.
  ; Therefore various quotes appearing after @ get applied to whatever
  ; Clojure syntax tells them to (that is, to the form that
  ; immediately follows), not the whole Scribble form.

  (fact "stacked reader macros work"
   '@'~@@foo{blah}
    =>
   ''~@(foo ["blah"]))

  (fact "command can be any expression"
   '@(lambda (x) x){blah}
    =>
   '((lambda (x) x) ["blah"]))

  (fact "reader macros and expression-command work together"
   '@@(unquote foo){blah}
    =>
   '(@(unquote foo) ["blah"]))

  (fact "command and normal part can be omitted"
   '@{foo bar
      baz}
    =>
   '(["foo bar" "\n"
      "baz"]))

  (fact "a lone body part can be escaped"
   '@``{abcde}` => '(["abcde"]))

  (fact "a lone body part can be escaped with delimiters"
   '@``-abc{abcde}cba-` => '(["abcde"]))


  (fact "a comment form glues surrounding strings"
   '@foo{bar @;{some text
      with newlines
      or even valid nested expressions like @command[foo]{bar};} baz}
    =>
   '(foo ["bar  baz"]))

  (fact "a comment form does not break whitespace truncation"
   '@foo{bar @;{blah
      blah;}
          baz}
    =>
   '(foo ["bar" "\n" " " "baz"]))

  (fact "a comment from works in normal mode"
   '@foo[bar @;{comment}
      2]
    =>
   '(foo bar 2))

  ; A difference from the orignial Scribble:
  ; Since we allow multiple body parts after a command,
  ; they all get consumed.
  ; The original Scribble produces `((foo "bar") "baz")`.
  (fact "Scribble form as a command"
   '@@foo{bar}{baz}
    =>
   '(foo ["bar"] ["baz"]))


  ; Racket Expression Escapes

  (fact "the end of a standalone symbol is detected properly"
   '@foo{foo@bar.}
    =>
   '(foo ["foo" bar.]))

  (fact "text in an escaped expression is not merged with the surrounding text"
   '@foo{x@`"y"`z}
    =>
   '(foo ["x" "y" "z"]))

  (fact "a number as a standalone expression"
   '@foo{foo@3.}
    =>
   '(foo ["foo" 3.0]))

  (fact "a number as an escaped expression"
   '@foo{foo@`3`.}
    =>
   '(foo ["foo" 3 "."]))

  (fact "escaped expression with multiple forms is spliced"
   '@foo{x@`1 (+ 2 3) 4`y}
    =>
   '(foo ["x" 1 (+ 2 3) 4 "y"]))

  (fact "escaped expression with a line break"
   '@foo{x@`*
         *`y}
    =>
   '(foo ["x" *
      * "y"]))

  (fact "an empty escaped expression"
   '@foo{Alice@` `Bob@`
         `Carol}
    =>
   '(foo ["Alice" "Bob"
      "Carol"]))

  ; Spaces, Newlines, and Indentation

  (fact "eee"
   '@foo{
    }
    =>
   '(foo ["\n"]))

  (fact "ewew"
   '@foo{
    bar
    })

))


; These tests were added specifically to cover all the branches.
; Contain various corner cases.
(deftest test-reader-coverage (facts "about the coverage"

  (fact "EOF right after a Scribble form"
    (read-scribble "@foo{bar}")
    =>
    '(foo ["bar"]))

  (fact "EOF right after a Scribble form with an empty command and body parts"
    (read-scribble "@foo")
    =>
    'foo)

  ; Same behavior as (read-string "; normal comment")
  (fact "EOF right after a non-consuming comment"
    (read-scribble "@; one two three")
    =>
    (throws RuntimeException "EOF while reading"))

  (fact "EOF right after a consuming comment"
    (read-scribble "@;; one two three")
    =>
    (throws RuntimeException "EOF while reading"))

  (fact "almost finished beginning here-string (except for the `{`)"
   '@foo`--{`--abc}--`
    =>
   '(foo ["`--abc"]))

  (fact "almost finished ending here-string (except for the `}`)"
   '@foo`--{abc}--}--`
    =>
   '(foo ["abc}--"]))

  (fact "comment inside an escaped body part"
   '@foo`{abc `@; comment
          cba}`
    =>
   '(foo ["abc" "\n" "cba"]))

  (fact "final leading whitespace in an escaped body part"
   '@foo`{abc
    }`
    =>
   '(foo ["abc"]))

))


(deftest test-symbol-resolution (facts "about the symbol resolution"

  (fact "nil is not ignored"
   '@foo{aaa @nil bbb}
    =>
   '(foo ["aaa " nil " bbb"]))

  ; Add @NaN @Infinity @+Infinity @-Infinity to this test
  ; when CLJ-1074 gets merged.
  (fact "literals are resolved"
   '@foo{aaa @false bbb @true ccc @/}
    =>
   '(foo ["aaa " false " bbb " true " ccc " /]))

  (fact "known symbols are resolved"
    (let [formatter (fn [fmt]
           (fn [str-vec] (format fmt (clojure.string/join str-vec))))
          bf (formatter "*%s*")
          it (formatter "/%s/")
          ul (formatter "_%s_")
          text clojure.string/join]
      @text{@it{Note}: @bf{This is @ul{not} a pipe}.}
      =>
      "/Note/: *This is _not_ a pipe*."))

))


(deftest test-reader-exceptions (facts "about the reader exceptions"

  (fact "unexpected whitespace after the entry character"
    (read-scribble "@ foo{bar}")
    =>
    (throws "Unexpected whitespace at the start of a Scribble form"))

  (fact "unexpected EOF after the single entry character"
    (read-scribble "@")
    =>
    (throws "Unexpected EOF at the start of a Scribble form"))

  (fact "unexpected EOF after the entry character"
    (read-scribble "(def foo @")
    =>
    (throws "Unexpected EOF at the start of a Scribble form"))

  (fact "an exception is thrown if the symbol is invalid"
    (read-scribble "@foo::{abc}")
    =>
    (throws RuntimeException "Invalid token: foo::"))

  (fact "unexpected EOF in a body part"
    (read-scribble "@foo{abc")
    =>
    (throws "Unexpected EOF while reading a body part"))

  (fact "unexpected EOF in while reading a here-string"
    (read-scribble "@``--")
    =>
    (throws "Unexpected EOF while reading a here-string"))

  (fact "invalid characters in a here-string"
    (read-scribble "@foo`-@{some text}@-`")
    =>
    (throws "Here-string contains invalid characters"))


))
