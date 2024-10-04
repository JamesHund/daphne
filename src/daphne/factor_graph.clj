(ns daphne.factor-graph 
  (:require [daphne.desugar-let :refer [desugar-let]]))

(defn dispatch-transform
  "rho is environment, phi whether we are on the control flow path, exp current
  expression"
  [exp]
  (cond (or (number? exp)
            (nil? exp)
            (string? exp)
            (boolean? exp)
            (keyword? exp))
        :constant

        (symbol? exp)
        :variable

        (and (list? exp)
             (= (first exp) 'defn))
        :defn

        (and (list? exp)
             (= (first exp) 'let))
        :let

        (and (list? exp)
             (= (first exp) 'if))
        :if

        (and (list? exp)
             (= (first exp) 'sample))
        :sample

        (and (list? exp)
             (= (first exp) 'observe))
        :observe

        (list? exp)
        :application

        (seq? exp)
        :seq

        (map? exp)
        :map

        (vector? exp)
        :vector

        :else
        (throw (ex-info "Not supported." {:exp exp 
                                          }))))

(defmulti transform dispatch-transform)

(defmethod transform :constant
  [c]
  c)

(defmethod transform :variable
  [v]
  v)

(defmethod transform :vector
  [exp]
  (mapv transform exp))

(defmethod transform :seq
  [exp]
  (map transform exp))

;; Not 100% sure about this one
(defmethod transform :map
  [exp]
  (into {} (map (fn [[k v]] [k (transform v)]) exp)))

;; Let stays the same except for the assigned expression and the body
(defmethod transform :let
  [exp]
  (println exp)
  (let [[_ [v e] body] (desugar-let exp)
        transformed-e (transform e)
        transformed-body (transform body)]
    `(let [~v ~transformed-e] ~transformed-body)))

;; Sample and observe are the same except for the expression
(defmethod transform :sample
  [exp]
  (let [[_ e] exp
        transformed-e (transform e)]
    `(sample ~transformed-e)))

;; Observe is the same as sample
(defmethod transform :observe
  [exp]
  (let [[_ e1 e2] exp
        transformed-e1 (transform e1)
        transformed-e2 (transform e2)]
    `(observe ~transformed-e1 ~transformed-e2)))

;; If we transform the condition and the two branches
;; and wrap it inside a (sample (dirac (if ...)))
(defmethod transform :if
  [rho exp]
  (let [[_ cond e1 e2] exp
        transformed-cond (transform rho cond)
        transformed-e1 (transform e1)
        transformed-e2 (transform e2)]
    `(sample (dirac (if ~transformed-cond ~transformed-e1 ~transformed-e2)))))

;; 
(defmethod transform :defn
  [exp]
  (let [[_ name args body] exp
        transformed-body (transform body)]
    `(defn ~name ~args ~transformed-body)))

;; TODO: keep track of defined functions and treat defn as a special case
(defmethod transform :application
  [exp]
  (println "Application: " exp)
  (let [[f & args] exp
        transformed-f (transform f)
        transformed-args (map (fn [a] (transform a)) args)]
    `(sample (dirac (~transformed-f ~@transformed-args)))))

;; Converts source code to a transformed directed graph
;; Performs the transform operation
;; The transformed graph is later converted to a factor graph
(defn source-code-transformation
  "Transforms a given source code sequence into its transformed version
   according to the rules defined in the `transform` multimethod."
  [code]
  ;; Apply transform to each expression in the code
  (map transform code))

;; Takes in a directed graph data structure and converts to a factor graph
(defn directed-graph->factor-graph
  [graph]
  ()
  )

;; Removes unnecessary factors / variables
(defn simplify-factor-graph
  [factor-graph]
  ()
  )