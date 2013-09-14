(ns scribble.core-test
  (:use [midje.sweet])
  (:require [clojure.test :refer :all]
            [scribble.core :refer :all]))


; Unfortunately, the reader that reads from strings does not have
; line/column metadata.
; So in order for the whitespace truncation to work properly,
; we need to use the main reader.
(use-scribble)


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

  (fact "quotes in a text block"
   '@foo{blah "blah" (`blah'?)}
    =>
   '(foo ["blah \"blah\" (`blah'?)"]))

  (fact "a normal block and a text block"
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
  ; The starting indentation is taken from the next line.
  ; Same rules as before apply for the remaining lines.
  (fact "leading indentation and a starting newline are truncated"
   '@foo{
      blah blah
      yada yada
        ddd
    ttt
   }
    =>
   '(foo [
      "blah blah" "\n"
      "yada yada" "\n"
      "  " "ddd" "\n"
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

  (fact "whitespace is attached to the text at the ends of the text block"
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

  (fact "command part only in a text block"
   '@{blah @foo blah}
    =>
   '(["blah " foo " blah"]))

  ; ':' in identifiers has special meaning in Clojure, so changed it to '-'
  (fact "non-trivial command in a text block"
   '@{blah @foo- blah}
    =>
   '(["blah " foo- " blah"]))

  (fact "escaped command in a text block"
   '@{blah @|foo |- blah}
    =>
   '(["blah " foo "- blah"]))

  (fact "text mode starts right after an escaped command"
   '@{blah @|foo |[3] blah}
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

  (fact "the text block delimiter wrapped in a string"
   '@foo{A @"{" begins a block}
    =>
   '(foo ["A { begins a block"]))

  (fact "balanced text block delimiters do not require escaping"
   '@C{while (*(p++)) {
         *p = '\n';
      }}
    =>
   '(C ["while (*(p++)) {" "\n"
       "  " "*p = '\\n';" "\n"
       "}"]))

  ; A regression for a bug in text reader logic
  (fact "text block delimiters at the beginning of text mode work correctly"
   '@foo{{{}}{}}
    =>
   '(foo ["{{}}{}"]))

  ; Here strings

  (fact "unbalanced text block delimiters in an escaped text block"
   '@foo|{bar}@{baz}|
   =>
   '(foo ["bar}@{baz"]))

  (fact "balanced escaped text block delimiters in an escaped text block"
   '@foo|{Nesting |{is}| ok}|
    =>
   '(foo ["Nesting |{is}| ok"]))

  (fact "an escaped nested command in an escaped text block"
   '@foo|{bar |@x{X} baz}|
    =>
   '(foo ["bar " (x ["X"]) " baz"]))

  (fact "an escaped nested text block in an escaped text block"
   '@foo|{bar |@x|{@}| baz}|
    =>
   '(foo ["bar " (x ["@"]) " baz"]))

  ; check that there is no off-by-one error because the delimiter is
  ; two symbols instead of one
  (fact "an escaped text block truncates leading indentation properly"
   '@foo|{Maze
          |@bar{is}
         Life!
           blah blah}|
    =>
   '(foo ["Maze" "\n"
      (bar ["is"]) "\n"
      "Life!" "\n"
      " " "blah blah"]))

  (fact "a verbatim text block"
   '@foo|--{bar}@|{baz}--|
    =>
   '(foo ["bar}@|{baz"]))

  (fact "a verbatim text block with mirrored delimiters"
   '@foo|<(-[{bar}@|{baz}]-)>|
    =>
   '(foo ["bar}@|{baz"]))

  (fact "a verbatim text block with a nested form"
   '@foo|!!{X |!!@b{Y}...}!!|
    =>
   '(foo ["X " (b ["Y"]) "..."]))

  ; Empty blocks

  (fact "an empty normal block is ignored"
   '@foo[]{bar} => '(foo ["bar"]))
  (fact "an empty normal block and a missing text block result in a form"
   '@foo[] => '(foo))
  (fact "a single command is read as a symbol"
   '@foo => 'foo)
  (fact "an empty text block results in an empty text container"
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
   '@`'~@@foo{blah}
    =>
   '`'~@(foo ["blah"]))

  (fact "command can be any expression"
   '@(lambda (x) x){blah}
    =>
   '((lambda (x) x) ["blah"]))

  (fact "syntax quotes and expression-command work together"
   '@`(unquote foo){blah}
    =>
   '(`(unquote foo) ["blah"]))

  (fact "command and normal part can be omitted"
   '@{foo bar
      baz}
    =>
   '(["foo bar" "\n"
      "baz"]))

  (fact "a lone text part can be escaped"
   '@||{abcde}| => '(["abcde"]))

  (fact "a lone text part can be verbatim"
   '@||-abc{abcde}cba-| => '(["abcde"]))


  (fact "commented text form glues surrounding strings"
   '@foo{bar @;{some text
      with newlines
      or even valid nested expressions like @command[foo]{bar};} baz}
    =>
   '(foo ["bar  baz"]))

  (fact "commented text form does not break whitespace truncation"
   '@foo{bar @;{blah
      blah;}
          baz}
    =>
   '(foo ["bar" "\n" " " "baz"]))

  (fact "commented text works in normal mode"
   '@foo[bar @;{comment}
      2]
    =>
   '(foo bar 2))

  ; A difference from the orignial Scribble:
  ; Since we allow multiple text blocks after a command,
  ; they all get consumed.
  ; The original Scribble produces `((foo "bar") "baz")`.
  (fact "scribble form as a scribble command"
   '@@foo{bar}{baz}
    =>
   '(foo ["bar"] ["baz"]))


  ; Racket Expression Escapes

  (fact "the end of a standalone symbol is detected properly"
   '@foo{foo@bar.}
    =>
   '(foo ["foo" bar.]))

  (fact "text in a standalone expression is not merged with the surrounding text"
   '@foo{x@|"y"|z}
    =>
   '(foo ["x" "y" "z"]))

  (fact "a number as a standalone expression"
   '@foo{foo@3.}
    =>
   '(foo ["foo" 3.0]))

  (fact "a number as an escaped expression"
   '@foo{foo@|3 |.}
    =>
   '(foo ["foo" 3 "."]))

  (fact "escaped expression with multiple forms is spliced"
   '@foo{x@|1 (+ 2 3) 4 |y}
    =>
   '(foo ["x" 1 (+ 2 3) 4 "y"]))

  (fact "escaped expression with a line break"
   '@foo{x@|*
        * |y}
    =>
   '(foo ["x" *
      * "y"]))

  (fact "an empty escaped expression"
   '@foo{Alice@| |Bob@|
         |Carol}
    =>
   '(foo ["Alice" "Bob"
      "Carol"]))

))

(deftest test-symbol-resolution (facts "about the symbol resolution"

  (fact "nil is not ignored"
   '@foo{aaa @nil bbb}
    =>
   '(foo ["aaa " nil " bbb"]))

  (fact "literals are resolved"
   '@foo{aaa @false bbb @true ccc}
    =>
   '(foo ["aaa " false " bbb " true " ccc"]))

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


(defn read-scribble [s]
  (with-scribble (read-string s)))

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

))
