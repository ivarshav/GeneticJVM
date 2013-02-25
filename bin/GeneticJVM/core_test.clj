(ns GeneticJVM.core-test
  (:use clojure.test
        GeneticJVM.core))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest my-test
  (Main "I Love Clojure!"))

(deftest story-a
  (Main "This is a story about"))

(deftest story-b
  (Main "the beauty & the beast."))

(deftest story-c
  (Main "They really love each other <3 <3"))

(deftest story-d
  (Main "THE END"))

(deftest testP
  (Main "Happy Purim!"))
