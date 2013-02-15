(ns GeneticJVM.core)

;Fooling around
(defn welcome
  [x]
  (println "Welcome To The Clojure Age, " x))

;Real job:

(def abc ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" " " "!"])

(def PopulationSize 30)

;Create Person
;Population loop NOT TESTED! PErson loop works!
;Need to add CONJ to Population map anf create it
(dotimes [n PopulationSize]
  (
		(def Person [])
		(dotimes [n 15] 
		  (def person 
		    (conj person 
		          (rand-nth abc)
		    )
		  )
		)
  )
)