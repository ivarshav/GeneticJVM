(ns GeneticJVM.core)

;Daniel's Update 18/02
;NOTE: I used different approach: instead of using a vector and loop i used recursive and cond and by so recieved the needed action - CONJing person in population and char in persons!!

(def abc ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "!"])

;is-person-space? returns true when i equals to 1 or 6 which are the matching indices of the spaces in "I Love Clojure"
(defn is-person-space? [i] (or (= i 1) (= i 6)))

(defn getPersonChar
  [i]
  (cond
  (is-person-space? i) " "
  :else (rand-nth abc) 
  )
)

(def PersonLength 15)
(def PopulationSize 30)

;_InitPerson returns a random initial person vector
(defn _InitPerson [i person]
  (cond
    (= i PersonLength) person
    :else   (cons (getPersonChar i) (_InitPerson (+ i 1) person))
    )
  )

;_InitPopulation returns a random intial population vector
(defn _InitPopulation [i pop]
  (cond
    (= i PopulationSize) pop
    :else (cons {(_InitPerson 0 []) -1} (_InitPopulation (+ i 1) pop))
    ;we evaluate each person with -1 meanwhile... later it will have its fitness
    )
  )

(def InitPopulation (_InitPopulation 0 {}))
