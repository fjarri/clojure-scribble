;; Contains functions that allow you to tune the characters used for
;; different purposes in the reader.
(ns scribble.settings)


(defn whitespace?
  [c]
  (or (= c \space) (= c \tab)))


(deftype Settings [
  ^Character entry-char
  ^Character body-start-char
  ^Character body-end-char
  ^Character datum-start-char
  ^Character datum-end-char
  ^Character escape-start-char
  ^Character escape-end-char
  ^Character comment-char])

(defn- validate-settings
  "Checks the reader settings for conflicts."
  [^Settings settings]
  settings)

(defn make-settings
  "Creates a reader settings structure,
  and checks the characters used for conflicts.
  Parameters:

  - `entry-char`: the entry point to our custom reader.
    **Warning:** any default reader macro for this symbol will be unusable
    when Scribble is enabled.
  - `body-start-char`, `body-end-char`: mark the body part.
    Ideally these occur rarely in the text, otherwise a lot of escaping
    will have to be used.
  - `datum-start-char`, `datum-end-char`: mark the datum part.
    `datum-end-char` must be a macro-terminating character
    (if it's not, it has to be preceeded by whitespace
    when it's used to terminate the datum part).
  - `escape-start-char`, `escape-end-char`: used for escaping body part
    and splicing forms.
    If `escape-start-char` has a reader macro assigned, this macro will be
    unusable in the first form of the splice (because two escaping characters
    mark the beginning of an escaped body part).
    `escape-end-char` must be a macro-terminating character
    (if it's not, it has to be preceeded by whitespace
    when it's used to terminate the spliced form).
  - `comment-char` is used (with a preceding `entry-char`)
    to start line comments, newline-consuming comments, and multiline comments.

  Characters in every group may be equal, but should differ from all
  characters in other groups."
  [entry-char
   body-start-char
   body-end-char
   datum-start-char
   datum-end-char
   escape-start-char
   escape-end-char
   comment-char]
  (validate-settings
    (Settings. entry-char
               body-start-char
               body-end-char
               datum-start-char
               datum-end-char
               escape-start-char
               escape-end-char
               comment-char)))

(def default-settings (make-settings \@ \{ \} \[ \] \` \` \;))


(defn entry-char
  [^Settings settings]
  (.entry-char settings))

(defn inverse-char
  [c]
  (case c
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
