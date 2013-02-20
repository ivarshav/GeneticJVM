(ns GeneticJVM.core)

;Fooling around
(defn welcome
  [x]
  (println "Welcome To The Clojure Age, " x))

;Real job:
(def target "i love clojure!") 
(def abc [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z \!])
(def PersonLength 15)
(def PopulationSize 30)

;is-person-space? returns true when i equals to 1 or 6 which are the matching indices of the spaces in "i love clojure!"
(defn is-person-space? [i] (or (= i 1) (= i 6)))

(defn getPersonChar
  [i]
  (cond
  (is-person-space? i) \ ;space
  :else (rand-nth abc) 
  )
)

;_InitPerson returns a random initial person vector
(defn _InitPerson [i person]
  (cond
    (= i PersonLength) person
    :else  (cons (getPersonChar i) (_InitPerson (inc i) person)) ;lazy-seq
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

;sort the population map by fitness value
(defn sort_by_fitness [population] 
  (sort-by val < population ))

;selection of the best persons (10% of elitism rate)
(def elitism_rate 0.1) 
(def elitism_size (int (* elitism_rate PopulationSize)))

(defn elitism [i population buffer] 
  ;(println "i: " i )
  (cond
    (= i elitism_size) buffer
    :else (sort_by_fitness (merge (into {} #{(first population)}) (elitism (inc i) (rest population) buffer))) ) ;anonymus function
  )
;;call elitism- (elitism 0 (sort_by_fitness InitPopulation) {})

;mutation- change a letter in a person
(defn MutationPerson [person]
   (def rnum (rand-int 15))
   (println rnum)
  (assoc person rnum (getPersonChar rnum) )
  )

(defn Mate [i population buffer]
  (def restp (take-last (- PopulationSize elitism_size) InitPopulation)) ;the rest of population
  
  )

