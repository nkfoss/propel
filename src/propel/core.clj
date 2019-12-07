; (use 'propel.core)
;;(-main)

(ns propel.core
  (:require [propel.utilities :as utl]
            ;[propel.instructions :as inst]
            [propel.interpreter :as inter]
            [clojure.string :as str]))

(def default-instructions
  (list
   'in1
   'exec_dup
   'exec_if
  ;  'boolean_and
  ;  'boolean_or
  ;  'boolean_not
  ;  'boolean_=
   'string_=
   'string_concat
   'string_length
   'string_removechar
   'close
   'string_removechar
   0
   1
   true
   false
   "a"
   "e"
   "i"
   "o"
   "u"
   "A"
   "E"
   "I"
   "O"
   "U"))


;; Instructions
(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (utl/push-to-stack state :exec (:in1 (:input state))))

(defn integer_+
  [state]
  (utl/make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  [state]
  (utl/make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  [state]
  (utl/make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  [state]
  (utl/make-push-instruction state
                             (fn [int1 int2]
                               (if (zero? int2)
                                 int1
                                 (quot int1 int2)))
                             [:integer :integer]
                             :integer))

(defn integer_=
  [state]
  (utl/make-push-instruction state = [:integer :integer] :boolean))

(defn exec_dup
  [state]
  (if (utl/empty-stack? state :exec)
    state
    (utl/push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (utl/make-push-instruction state
                             #(if %1 %3 %2)
                             [:boolean :exec :exec]
                             :exec))

(defn boolean_and
  [state]
  (utl/make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (utl/make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (utl/make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (utl/make-push-instruction state = [:boolean :boolean] :boolean))

(defn string_=
  [state]
  (utl/make-push-instruction state = [:string :string] :boolean))

(defn string_take
  [state]
  (utl/make-push-instruction state
                             #(apply str (take %1 %2))
                             [:integer :string]
                             :string))

(defn string_drop
  [state]
  (utl/make-push-instruction state
                             #(apply str (drop %1 %2))
                             [:integer :string]
                             :string))

(defn string_reverse
  [state]
  (utl/make-push-instruction state
                             #(apply str (reverse %))
                             [:string]
                             :string))

(defn string_concat
  [state]
  (utl/make-push-instruction state
                             #(apply str (concat %1 %2))
                             [:string :string]
                             :string))

(defn string_length
  [state]
  (utl/make-push-instruction state count [:string] :integer))


; (defn string_removechar ; In top string on stack, remove all occurences of char
;   ^{:stack-types [:string :char]}
;   (fn [state]
;     (if (and (not (empty? (:string state)))
;              (not (empty? (:char state))))
;       (let [result (apply str (remove #{(stack-ref :char 0 state)}
;                                       (stack-ref :string 0 state)))]
;         (push-item result
;                    :string
;                    (pop-item :char (pop-item :string state))))
;       state)))

(defn string_removechar
  [state]
  (utl/make-push-instruction state
                             #(apply str (remove #{%1} %2))
                             [:char :string]
                             :string))

(defn disemvowel
  [string]
  (apply str
         (remove #{\a \e \i \o \u \A \E \I \O \U}
                 string)))


(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))

(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap)))
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap)))))})

(defn report
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (inter/push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println)))

(defn propel-gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size]
    :as argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                     population-size
                     #(hash-map :plushy
                                (make-random-plushy instructions
                                                    max-initial-plushy-size)))]
    (let [evaluated-pop (sort-by :total-error
                                 (map (partial error-function argmap)
                                      population))]
      (report evaluated-pop generation)
      (cond
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size 
                                 #(new-individual evaluated-pop argmap)))))))

(defn target-function
  "Our disemvowel function"
  [string]
  (apply str
         (remove #{\a \e \i \o \u \A \E \I \O \U}
                 string)))


(defn compute-next-row
  [prev-row current-element other-seq pred]
  (reduce
   (fn [row [diagonal above other-element]]
     (let [update-val (if (pred other-element current-element)
                        diagonal
 
                        (inc (min diagonal above (peek row))))]

       (conj row update-val)))

   [(inc (first prev-row))]
   (map vector prev-row (next prev-row) other-seq)))

(defn levenshtein-distance
  [a b & {p :predicate  :or {p =}}]
  (cond
    (= b :no-stack-item) 100
    (empty? a) (count b)
    (empty? b) (count a)
    :else (peek
           (reduce
            
            (fn [prev-row current-element]
              (compute-next-row prev-row current-element b p))
            (range (inc (count b)))
            a))))

(defn regression-error-function
  "Finds the behaviors and errors of the individual."
  [argmap individual]
  (let [program (inter/push-from-plushy (:plushy individual))
        inputs (range -10 11)
        correct-outputs (map target-function inputs)
        outputs (map (fn [input]
                       (utl/peek-stack
                        (inter/interpret-program
                         program
                         (assoc utl/empty-push-state :input {:in1 input})
                         (:step-limit argmap))
                        :integer))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (utl/abs (- correct-output output))))
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply +' errors))))

(defn disemvowel-error-function
  "Finds the behaviors and errors of the individual."
  [argmap individual]
  (let [program (inter/push-from-plushy (:plushy individual))
        inputs (str/split (slurp "dictionary.txt") #"\n") ;; Get from a text file
        correct-outputs (map target-function inputs) ;; Apply our disemvowel fxn
        outputs (map (fn [input]
                       (utl/peek-stack
                        (inter/interpret-program
                         program
                         (assoc utl/empty-push-state :input {:in1 input})
                         (:step-limit argmap))
                        :string))
                     inputs)
        errors (map levenshtein-distance
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply +' errors))))


(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'propel.core)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function disemvowel-error-function
                                  :max-generations 100
                                  :population-size 200
                                  :max-initial-plushy-size 50
                                  :step-limit 100
                                  :parent-selection :tournament
                                  :tournament-size 5}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))