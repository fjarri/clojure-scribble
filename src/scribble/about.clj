;; # Differences from Scribble
;;
;; The syntax differs from
;; [Racket's Scribble](http://docs.racket-lang.org/scribble/reader.html)
;; in several ways, partly to accommodate for Clojure syntax,
;; partly for other reasons described below.
;;
;; First, to make the syntax slightly more general, any number of
;; datum and body parts are allowed in the Scribble form.
;; This was originally intended to allow mapping of multi-argument TeX macros
;; such as `\frac{}{}` easily, but may be useful in other cases.
;; For example, `@foo[:bar 2]{baz}{blah}` reads as
;; `(foo :bar 2 ["baz"] ["blah"])`.
;;
;; As a result, we cannot just splice the contents of a body part into
;; the parent Scribble form anymore.
;; Every body form is thus wrapped in a vector (see the example above).
;; Another consequence is that `@foo{}` reads as `(foo [])` and not as `foo`
;; (if you want the latter, you may use just `@foo` or escaped `` @`foo` ``).
;;
;; The next difference is caused by the implementation.
;; Any reader macros that follow the `@` are *not* applied to the whole
;; Scribble form that starts with this `@`; rather they are applied
;; to the form that follows them according to Clojure syntax.
;; Our reader gets control only when the Clojure reader finishes,
;; and uses the resulting form as a command.
;;
;; Since the same symbol is used for spliced commands and for escaped string,
;; it is impossible to distinguish whether `` @`- `` starts a list of forms
;; (e.g. `` @`- 3 2` ``), or an escaped string (like `` @`-{blah}-` ``).
;; Therefore the escaped strings at the start of a Scribble form must
;; use two escaping characters: ``` @``-{blah}-` ```.
;;
;; Following from the previous difference, the empty spliced command must
;; contain at least one whitespace symbol: ``@foo{Alice@` `Bob}``.
;;
;; Finally, `@;` consumes characters only till the newline;
;; the newline-consuming comment is written as `@;;`.
;; This was done because the newline-consuming comments
;; are not needed too often, and can be confusing.
;;
(ns scribble.about)

; Marginalia sorts namespaces in alphabetical order,
; so we use this one to store the general part of the documentation.
