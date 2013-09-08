# Scribble syntax in Clojure

[Scribble](http://docs.racket-lang.org/scribble/) is a documentation generation tool based on a [text-oriented syntax](http://docs.racket-lang.org/scribble/reader.html) for S-expressions.
This is a Clojure module implementing the syntax (via a reader macro), not the whole documentation generator.
See [the project documentation page](http://clojure-scribble.publicfields.net) for more details.

## Usage

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
      [:require [scribble.core :as scribble]])

    (def forms
      (with-scribble
        (read (java.io.PushbackReader.
          (clojure.java.io/reader "source.clj")))))

   Note that if the reader does not provide line/column metadata, the leading whitespace truncation will not be applied to the text blocks.
