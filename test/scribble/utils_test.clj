(ns scribble.utils-test
  (:require [clojure.test :refer :all]
            [scribble.utils :refer :all]))


; Tests for find-last

(def testcases-find-last
  [
    ["abc" 2]
    ["   \tabc" 6]
    [" \t abc " 5]
    ["   a b c \t d " 11]
    ["" nil]
    ["  \t" nil
    ["a     " 0]]])

(defn find-last-non-ws [v]
  (find-last #(not (whitespace? %)) v))

(deftest test-find-last
  (doseq [[s expected-pos] testcases-find-last]
    (testing (repr s)
      (is (= (find-last-non-ws s) expected-pos)))))


; Tests for dump-accum

(def testcases-dump-accum
  [
    ["abc" false ["abc"]]
    ["abc" true ["abc"]]
    ["  abc \t" false ["  abc \t"]]
    ["  abc \t" true ["  abc" " \t"]]
    ["" false []]
    ["" true []]
    ["    " false ["    "]]
    ["    " true ["    "]]])

(defn split-and-dump [s separate-trailing-ws]
  (let [vec-accum []
        str-accum (vec s)]
    (dump-accum vec-accum str-accum separate-trailing-ws)))

(deftest tets-dump-accum
  (doseq [[s separate-trailing-ws expected-vec] testcases-dump-accum]
    (let [result-vec (split-and-dump s separate-trailing-ws)]
      (testing (repr s)
        (is (= result-vec expected-vec))))))
