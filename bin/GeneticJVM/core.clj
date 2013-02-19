(ns GeneticJVM.core)

;Fooling around
(defn welcome
  [x]
  (println "Welcome To The Clojure Age, " x))

;Real job:
(def target "i love clojure!") 
(def abc [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z \!])

;is-person-space? returns true when i equals to 1 or 6 which are the matching indices of the spaces in "i love clojure!"
(defn is-person-space? [i] (or (= i 1) (= i 6)))

(defn getPersonChar
  [i]
  (cond
  (is-person-space? i) \ ;space
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

;calc the fitness of a person,returns map of the person and his fitness
(def fitness 0)
(defn calc_fitness [person target] 
(letfn [(fit [fitness p t]
         (if (empty? (rest p) )
           {person fitness}
           (recur (+ fitness (Math/abs (- (int (first p)) (int(first t))))) (rest p) (rest t))))]
  (fit 0 person target))
  )  

;_InitPopulation returns a random intial population vector
(defn _InitPopulation [i pop]
 ;(println "i: " i "a" (_InitPerson 0 []))
  (cond
    (= i PopulationSize) pop
    :else (merge (calc_fitness (_InitPerson 0 []) target) (_InitPopulation (+ i 1) pop)))
  )
(def InitPopulation (_InitPopulation 0 {}))