(ns scribble.settings-test
  (:use [midje.sweet])
  (:require [clojure.test :refer :all]
            [scribble.settings :refer :all]))


(deftest test-validation (facts "about the settings validation"

  (fact "characters cannot be whitespace"
    (make-settings \@ \space \} \[ \] \| \| \;)
    =>
    (throws "None of the characters can be whitespace or newlines"))

  (fact "characters used for different purposes cannot be equal"
    (make-settings \@ \{ \} \[ \] \| \| \@)
    =>
    (throws "Characters used for different purposes must be different"))

  (fact "characters used for the same purpose can be equal"
    (make-settings \@ \{ \} \[ \] \| \| \;)
    =>
    truthy)

))
