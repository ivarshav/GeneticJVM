(ns GeneticJVM.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn welcome01234
  [x,y]
  (println x y))

(defn welcome
  [x]
  (welcome01234 "Welcome To The Clojure Age, " x))