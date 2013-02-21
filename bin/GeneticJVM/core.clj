(ns GeneticJVM.core)
;Fooling around
(defn welcome
  [x]
  (println "Welcome To The Clojure Age, " x))

;Real job:
(def target "i love clojure!") 
(def abc [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z \!])
(def PersonLength 15)
(def PopulationSize 10)

;is-person-space? returns true when i equals to 1 or 6 which are the matching indices of the spaces in "i love clojure!"
(defn is-person-space? [i] (or (= i 1) (= i 6)))

(defn getPersonChar
  [i]
  (cond
  (is-person-space? i) \ ;space
  :else (rand-nth abc))
)

;_InitPerson returns a random initial person vector
(defn _InitPerson [i]
  (cond
    (= i PersonLength) [] 
    :else  (vec (cons (getPersonChar i) (_InitPerson (inc i) )))) ;lazy-seq
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
(defn _InitPopulation [i]
 ;(println "i: " i "a" (_InitPerson 0 ))
  (cond
    (= i PopulationSize) {}
    :else (merge (calc_fitness (_InitPerson 0 ) target) (_InitPopulation (+ i 1))))
  )
(def InitPopulation (_InitPopulation 0)) 

;sort the population map by fitness value
(defn sort_by_fitness [population] 
  (sort-by val < population ))

;selection of the best persons (10% is the elitism rate)
(def elitism_rate 0.1) 
(def elitism_size (int (* elitism_rate PopulationSize)))
(def rest_elitism_size (- PopulationSize elitism_size)) 

(defn elitism [i population] 
  ;(println "i: " i )
  (cond
    (= i elitism_size) {} 
    :else (merge (into {} #{(first population)}) (elitism (inc i) (rest population)))) ;convert from vector to map
  )
;;call elitism- (elitism 0 (sort_by_fitness InitPopulation))

;(defn take-greatest-vals [n m]
      ;  (when-let [entries (seq m)]
       ;   (reduce (fn [best x]
        ;            (if (>= (val x) (val (last best)))
         ;             (vec (take n (conj best x)))
          ;            best))
           ;      [(first entries)] (rest entries))))

;(into {} (take-greatest-vals 10 (sort_by_fitness InitPopulation)))

;mutation- change a letter in a person
(defn MutationPerson [person]
   (def rnum (rand-int PersonLength)) ;(println "mutaion:" rnum person)
  (assoc person rnum (getPersonChar rnum) )
  )

;cross-over between two parents
(defn cross-over [parent1 parent2]
   (def rnum (rand-int PersonLength))  
   ;(println "cross-over" rnum parent1 parent2)
   (vec (flatten (cons (take rnum parent1) (take-last (- PersonLength rnum) parent2))))
   
   )

;Random Parent in form of vector-select a Parent
(defn Rand_Parent [population]
  (nth (keys population) (rand-int (int (/ PopulationSize 2))))) ;choosing randomic person from the better half of the population for mating

(def MutationRate 0.25)
(def RandMax 40)
(def GA_Mutation (* MutationRate RandMax))

;(def restp (take-last (- PopulationSize elitism_size) (sort_by_fitness InitPopulation)))
; (def restp (take-last (- PopulationSize elitism_size) population)) ;the rest of population
;returns a map of one child and his fitness
(defn Mate [parent1 parent2]
  (calc_fitness (if (< (rand-int RandMax) GA_Mutation) ((comp MutationPerson cross-over) parent1 parent2)  
    (cross-over parent1 parent2)) target) ;getting new infant +mutaion if needed , in a form of key+fitness value
  )

;create a coll size of rest_elitism_size
(defn Selection [population]
  (letfn [(select [i person]
                  (if (= i rest_elitism_size) person 
                    (recur (inc i) (vec(cons (Rand_Parent population) person)))))]
  (select 0 (vector (Rand_Parent population))))
  )

;creates the new population (elitism+ the mate of the rest)
(defn Mate2 [population]
  (def Elite (elitism 0 (sort_by_fitness population)))
  (pmap Mate (Selection population) (Selection population)))