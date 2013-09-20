# Scribble syntax in Clojure

[Scribble](http://docs.racket-lang.org/scribble/) is a documentation generation tool based on a [text-oriented syntax](http://docs.racket-lang.org/scribble/reader.html) for S-expressions.
This is a Clojure module implementing the syntax (via a reader macro), not the whole documentation generator.
See [the full annotated source](http://clojure-scribble.publicfields.net) for more details on the implementation.


## Differences from Scribble

The syntax differs from the Racket's Scribble in several ways, partly because of the Clojure syntax restrictions, partly for other reasons described below.

First, the character used for escaping is not `|`, but `` ` ``, because it is macro-terminating in Clojure.
See the "Settings" section for more information about tuning the significant characters.

To make the syntax slightly more general, any number of datum and body parts are allowed in the Scribble form.
This was originally intended to allow mapping of multi-argument TeX macros such as `\frac{}{}` easily, but may be useful in other cases.
For example, `@foo[:bar 2]{baz}{blah}` reads as `(foo :bar 2 ["baz"] ["blah"])`.

As a result, we cannot just splice the contents of a body part into the parent Scribble form anymore.
Every body form is thus wrapped in a vector (see the example above).
Another consequence is that `@foo{}` reads as `(foo [])` and not as `foo` (if you want the latter, you may use just `@foo` or escaped `` @`foo` ``).

The next difference is caused by the implementation.
Any reader macros that follow the `@` are *not* applied to the whole Scribble form that starts with this `@`; rather they are applied to the form that follows them according to Clojure syntax.
Our reader gets control only when the Clojure reader finishes, and uses the resulting form as a command.

Since the same symbol is used for spliced commands and for the escaped string, it is impossible for the reader to distinguish whether `` @`- `` starts a list of forms (e.g. `` @`- 3 2` ``), or an escaped string (like `` @`-{blah}-` ``).
Therefore the escaped strings at the start of a Scribble form must use two escaping characters: ``` @``-{blah}-` ```.

Following from the previous difference, the empty spliced command must contain at least one whitespace symbol: ``@foo{Alice@` `Bob}``.

Finally, `@;` consumes characters only till the newline; the newline-consuming comment is written as `@;;`.
This was done because the newline-consuming comments are not needed too often, and can be confusing.


## Basic usage

Add ``[net.publicfields/scribble "0.1.0"]`` to your project's dependencies.
The reader macro can be used in two ways:

1. Namespace-wide extension of the reader:

        (ns your.project
          [:require [scribble.core :refer :all]])

        (use-scribble)

        ; Now you can use the new syntax!
        @def[xx]{This is a string}

        (println xx)

2. Temporary extension of the reader:

        (ns your.project
          [:require [scribble.core :refer :all]])

        (def forms
          (with-scribble
            (read-string "@foo{bar}")))

   Note that if the reader does not provide line/column metadata (for example, when you are reading from a string), the leading whitespace truncation will not be applied to the text blocks.


## Settings

The reader's behavior can be altered by passing an optional ``Settings`` object to ``use-scribble`` or ``with-scribble``.
The settings object allows one to change the characters that are used for different parts of the syntax.
A settings object identical to the default one can be defined as

    (ns your.project
      [:require [scribble.settings :refer :all]])

    (def my-settings
      (make-settings \@ \{ \} \[ \] \` \` \;))

and used as

    (use-scribble my-settings)

or, as a macro

    (with-scribble-settings my-settings
      (read-string "..."))

The settings object performs a basic validation (such as checking that the characters used for different purposes are different), but at the moment it does not check whether the characters passed are actually macro-terminating (that is, are already registered in the reader's macro table).
If they are not, you will have to prefix them with whitespace, so that Clojure does not treat them as part of the preceeding tokens.

This does not mean that all the characters in used in ``Settings`` are masked completely and cannot be used for their usual functions in Clojure.
In fact, the only character that is fully masked is the entry character `@`; plus, the escape-start and body-start characters `` ` `` and `{` cannot be used at the start of the command part (for instance, `@(:a 1)` will be read as a list `(:a 1)`, but `@{:a 1}` will be read as a body part `([":a 1"])`).


## TODO

There are several things that need to be done before this library can be safely assigned the ``1.0.0`` version:

- Default characters for the reader need to be established.
  The current ones were only chosen to coincide with the original Scribble.
- The container for the body part may be changed as well (or even become tunable through ``Settings``).
- Currently when a ``Settings`` object is created, it does not check whether the characters passed to it are macro-terminating.
  This can lead to unexpected behavior in some cases.
- To fix the previous issue, ``Settings`` may assign a stub reader macro for characters that are not currently in the reader macro table.
- In order for this library to be used for TeX generation, `_` and `^` symbols need to be processed differently in the body part (to allow conversions like ``@equation{e^{ix}}`` to ``(equation ["e" (sup ["ix"])])``).
  This can be tunable through ``Settings``.


## Contributing

If you make a pull request, depending on its nature, consider doing some of the following:

- running ``lein checkall`` for basic sanity checks of the code;

- running the test suit via ``lein midje`` (consider also adding your own tests if necessary);

- running the coverage testing via ``lein cloverage``;

- running the performance tests before and after your change via ``lein perforate`` (currently only a single testcase).

