(ns GeneticJVM.core)

;; WE HAVE RANDOM TARGET!
(def target) ;because the functions use this definition, we define it in Main
(def PersonLength) ;target length ;because the functions use this definition, we define it in Main
(def PopulationSize 3000)
(def GA_MaxIterations 16384) ;the number of generations

(defn getPersonChar []  ;rand char between \space to \z
  (char (+ (rand-int 90) 32)))

;_InitPerson returns a random initial person vector
(defn _InitPerson [i]
  (cond
    (= i PersonLength) [] 
    :else  (vec (cons (getPersonChar ) (_InitPerson (inc i) ))))
  )
  
;calc the fitness of a person,returns map of the person and his fitness
(def fitness 0)
(defn calc_fitness [person target] 
(letfn [(fit [fitness p t]
         (if (empty? p )
           {person fitness}  
           (recur (+ fitness (Math/abs (- (int (first p)) (int(first t))))) (rest p) (rest t))))]
  (fit 0 person target))
  )  

;_InitPopulation returns a random intial population vector
(defn _InitPopulation [i]
  (cond
    (= i PopulationSize) {}
    :else (merge (calc_fitness (_InitPerson 0 ) target) (_InitPopulation (+ i 1))))
  )

;sort the population map by fitness value in a form of a map
(defn sort_by_fitness [population] 
  (into (sorted-map-by (fn [key1 key2] (compare [(population key1) key1] [(population key2) key2]))) population))

;selection of the best persons (10% is the elitism rate)
(def elitism_rate 0.1) 
(def elitism_size (int (* elitism_rate PopulationSize)))
(def rest_elitism_size (- PopulationSize elitism_size)) 

(defn elitism [i population] 
  (cond
    (= i elitism_size) {} 
    :else (merge (into {} #{(first population)}) (elitism (inc i) (rest population)))) ;convert from vector to map
  )

;mutation- change a letter in a person
(defn MutationPerson [person]
   (def rnum (rand-int PersonLength)) ;(println "mutaion:" rnum person)
  (assoc person rnum (getPersonChar ) )
  )

;cross-over between two parents
(defn cross-over [parent1 parent2]
   (def rnum (rand-int PersonLength))  
   (vec (flatten (cons (take rnum parent1) (take-last (- PersonLength rnum) parent2))))   
   )

;Random Parent in form of vector-select a Parent
(defn Rand_Parent [population]
  (nth (keys population) (rand-int (int (/ PopulationSize 2))))) ;choosing randomic person from the better half of the population for mating

(def MutationRate 0.25)
(def RandMax 40)
(def GA_Mutation (* MutationRate RandMax))

;returns a map of one child and his fitness
(defn Mate [parent1 parent2]
  (calc_fitness (if (< (rand-int RandMax) GA_Mutation) ((comp MutationPerson cross-over) parent1 parent2)  
    (cross-over parent1 parent2)) target) ;getting new infant +mutaion if needed , in a form of key+fitness value
  )

;create a coll size of rest_elitism_size,returns a vector of Perosns from the better half of the population
(defn Selection [population]
  (letfn [(select [i person]
                  (if (= i rest_elitism_size) person 
                    (recur (inc i) (vec(cons (Rand_Parent population) person)))))]
  (select 0 (vector (Rand_Parent population))))
  ) 

;creates the new population (elitism+ the mate of the rest)
(defn Mate2 [population]
  (def Elite (elitism 0 population))
  (def restMate (into {} (map Mate (Selection population) (Selection population))))
  (sort_by_fitness (merge Elite restMate))
  )

(defn Main [input]
  (def target input)
  (def PersonLength (count target))
  (println "Creating Initial Population")
  (def InitPopulation (_InitPopulation 0))
  (def Sorted_Initpop (sort_by_fitness InitPopulation))
  ;print new population as strings   NewPopulation -not sure if need
  (loop [i 0 population Sorted_Initpop] 
  (println "Iteration" i "best: " (first population))   ;(print_best[(first population)]
  (if  (= (first (vals population)) 0) (println "we got it!   " (first (keys population)))  ;print "i love clojure!"
  (if  (= i GA_MaxIterations) (println "sorry..") ;didnt get our target
  (recur (inc i) (Mate2 population)))))
  )