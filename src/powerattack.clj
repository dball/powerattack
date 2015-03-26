(ns powerattack
  (:require [instaparse.core :as insta]))

(def damage-grammar
  "<expr> = scalar | expr <ws?> <'+'> <ws?> scalar
   scalar = (number | die-roll) (<ws+> type)?
   number = #'[1-9][0-9]*'
   ws = #'\\s+'
   <die-face> = '3' | '4' | '6' | '8' | '10' | '12' | '20' | '100'
   die-roll = number? <'d'> die-face
   type = 'cold' | 'fire' | 'sonic' | 'force' | 'acid' | 'electricity' | 'holy' | 'unholy' | 'good' | 'evil' | 'lawful' | 'chaotic'")

(def parse-damage
  (insta/parser damage-grammar))

(defprotocol Observer
  (observe! [this die]))

(extend-protocol Observer
  java.util.Random
  (observe! [random die]
    (-> random .nextInt (mod die) inc)))

(def maximizer
  (reify
    Observer
    (observe! [_ die]
      die)))

(defprotocol Observable
  (observe [this context]))

(defrecord DieRolls [number die]
  Object
  (toString [_]
    (str (when (> number 1) number) "d" die))
  Observable
  (observe [_ observer]
    (reduce + (repeatedly number #(observe! observer die)))))

(defn expand-die-rolls
  [die-rolls]
  (let [{:keys [number die]} die-rolls]
    (loop [pool #{0}
           number-left number]
      (if (= 0 number-left)
        pool
        (let [this-roll (map inc (range die))
              next-pool (for [i pool
                              j this-roll]
                          (+ i j))]
          (recur next-pool (dec number-left)))))))

(defrecord FixedDamage [value type]
  Object
  (toString [_]
    (str value (when type " ") type))
  Observable
  (observe [this _] this))

(defrecord VariableDamage [die-rolls type]
  Observable
  (observe [this context]
    (->FixedDamage (observe die-rolls context) type)))

(defn reduce-damage
  [pool damage]
  (let [{:keys [value type]} damage]
    (update-in pool [type] (fnil + 0) value)))

(defn sum-damages
  [damages]
  (reduce reduce-damage {} damages))

(defn build-die-rolls
  [expr]
  (reduce (fn [die-rolls expr]
            (if (= :number (first expr))
              (assoc die-rolls :number (Integer/parseInt (second expr)))
              (assoc die-rolls :die (Integer/parseInt expr))))
          (->DieRolls 1 nil)
          expr))

(defn reduce-damage-expr
  [damage expr]
  (cond (= :number (first expr))
        (->FixedDamage (Integer/parseInt (second expr)) nil)

        (= :die-roll (first expr))
        (->VariableDamage (build-die-rolls (rest expr)) nil)

        (= :type (first expr))
        (assoc damage :type (second expr))))

(defn eval-damage
  ([s] (eval-damage (java.security.SecureRandom.) s))
  ([observer s]
     (let [exprs (parse-damage s)
           damages (map (partial reduce reduce-damage-expr) exprs)
           actuals (map #(observe % observer) damages)]
       (prn (sum-damages actuals)))))
