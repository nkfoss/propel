(ns propel.instructions
  (:require [propel.utilities :as utl]))

;; Instructions
(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack."
  [state]
  (utl/push-to-stack state :exec (:in1 (:input state))))

(defn integer_structio
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

(defn string_includes?
  [state]
  (utl/make-push-instruction state clojure.string/includes? [:string :string] :boolean))