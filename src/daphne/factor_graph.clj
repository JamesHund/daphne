(ns daphne.factor-graph 
  (:require [clojure.set :as set]
            [daphne.desugar-let :refer [desugar-let]]))

(def distributions
  #{'dirac 'normal 'beta 'gamma 'dirichlet 'uniform 'uniform-continuous 'categorical 'discrete 'exponential 'poisson 'bernoulli 'binomial 'multinomial})
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

        (and (list? exp)
             (contains? distributions (first exp)))
        :dist

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
  (let [[_ [v e] body] (desugar-let exp)
        transformed-e (transform e)
        transformed-body (transform body)]
    (list 'let [v transformed-e] transformed-body)))

;; Sample and observe are the same except for the expression
(defmethod transform :sample
  [exp]
  (let [[_ e] exp
        transformed-e (transform e)]
    (list 'sample transformed-e)))

;; Observe is the same as sample
(defmethod transform :observe
  [exp]
  (let [[_ e1 e2] exp
        transformed-e1 (transform e1)
        transformed-e2 (transform e2)]
    (list 'observe transformed-e1 transformed-e2)))

;; If we transform the condition and the two branches
;; and wrap it inside a (sample (dirac (if ...)))
(defmethod transform :if
  [exp]
  (let [[_ cond e1 e2] exp
        transformed-cond (transform cond)
        transformed-e1 (transform e1)
        transformed-e2 (transform e2)]
    (list 'sample (list 'dirac (list 'if transformed-cond transformed-e1 transformed-e2)))))

;; 
(defmethod transform :defn
  [exp]
  (let [[_ name args body] exp
        transformed-body (transform body)]
    (list 'defn name args transformed-body)))

(defmethod transform :dist
  [exp]
  (let [[dist & args] exp
        transformed-args (map transform args)]
    (apply list dist transformed-args)))

;; TODO: keep track of defined functions and treat defn as a special case
(defmethod transform :application
  [exp]
  (let [[f & args] exp
        transformed-f (transform f)
        transformed-args (map (fn [a] (transform a)) args)]
    (list 'sample (list 'dirac (apply list transformed-f transformed-args)))))

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
(defn graph->factor-graph [directed-graph]
  (let [{:keys [V A P Y]} (second directed-graph)  ;; Extract the graph structure
        observed-vars (keys Y)                     ;; Get the observed variables
        unobserved-vars (set/difference V (set observed-vars))  ;; Get unobserved variables
        factors (into {} (map (fn [v] [v (symbol (str "f-" (name v)))]) V))  ;; Map factors with corresponding variable names
        factor-nodes (vals factors)                 ;; List of factor nodes (values of factors map)
        variable-nodes unobserved-vars              ;; Variable nodes are the unobserved variables
        edges (into {}                               ;; Create edges mapping
                    (map (fn [[v _]]
                           [v (get factors v)])   ;; Map from variable to factor
                         (for [v (keys A) 
                               :let [v-factors (A v)]]
                           [v (set (map (fn [v-f] (get factors v-f)) v-factors))]))) ;; Ensure mapping to actual factors
                     ;; A is the adjacency mapping from directed graph
        psi (into {}                                 ;; Construct psi mappings
                  (map (fn [v]
                         [(get factors v) (if (contains? Y v)  ;; Check if the variable is observed
                              (get P v)           ;; Substitute observed value into expression
                              (get P v))])        ;; Keep expression as is if not observed
                       (keys factors)))]           ;; Iterate over factors to create psi
    ;; Return the transformed factor graph
    [                                   
      ;; Original user-defined functions and return expression
      (first directed-graph)            ;; User-defined functions
      {:X (vec variable-nodes)          ;; X: unobserved variables (as vector)
       :F (vec factor-nodes)             ;; F: factor nodes (as vector)
       :A edges                           ;; A: undirected edges
       :psi psi                           ;; psi: map of factors
       }
      (last directed-graph)              ;; Return expression
    ]))

(defn factor->variable
  "Extracts the variable name from a factor name by removing the 'f-' prefix."
  [factor]
  (symbol (subs (name factor) 2)))  ;; remove 'f-' prefix and return as symbol

(defn simplify-psi
  "Simplifies the psi map by removing unnecessary 'sample*' and 'observe*',
   and properly formatting the distribution expressions."
  [psi]
  (into {}
        (for [[factor expr] psi]
          (let [[operation arg1 & rest-args] expr  ;; Correctly handle the first argument (the distribution)
                var-name (factor->variable factor)]  ;; Extract variable name from factor name
            ;; Simplify based on operation type
            (cond
              ;; If the operation is 'sample*', format it as [distribution-name, var-name, distribution-args...]
              (= operation 'sample*)
              (let [[dist-name & dist-args] arg1]  ;; arg1 is the distribution expression
                [factor (into [dist-name var-name] dist-args)])

              ;; If the operation is 'observe*', format it as [distribution-name, variable-in-dist, observed-value]
              (= operation 'observe*)
              (let [[dist-name & dist-args] arg1
                    observed-value (first rest-args)
                    variable-in-dist (first dist-args)]  ;; Get variable from distribution args
                [factor [dist-name variable-in-dist observed-value]])

              ;; Otherwise, return as-is for other operations like 'dirac'
              :else
              [factor expr])))))

(defn reformat-factor-graph
  "Simplifies the factor graph by applying the simplify-psi function to the 'psi' map,
   and returns the full data structure with just the graph replaced."
  [factor-graph]
  (let [graph (second factor-graph)
        psi (:psi graph)
        simplified-graph (assoc graph :psi (simplify-psi psi))]
    ;; Return the full structure with the updated graph
    [(first factor-graph) simplified-graph (last factor-graph)]))

;; Function to determine if a value is a variable
(defn is-variable? 
  "Checks if v is a variable (symbol in variables)."
  [v variables]
  (and (symbol? v) (contains? variables v)))

;; Function to determine if a value is a constant
(defn is-constant?
  "Checks if v is a constant (number, boolean, or symbol not in variables)."
  [v variables]
  (or (number? v) (boolean? v) (and (symbol? v) (not (contains? variables v)))))

;; Helper function to extract variables from expressions
(defn get-vars-in-expr 
  "Recursively collects symbols (variables) in an expression that are in the variables set."
  [expr variables]
  (cond
    (symbol? expr) (if (contains? variables expr) #{expr} #{})
    (coll? expr) (apply set/union (map #(get-vars-in-expr % variables) expr))
    :else #{}))

;; Function to update the adjacency list
(defn update-adjacency 
  "Builds the adjacency map from factors to variables based on psi and the variables set."
  [psi variables]
  (into {}
        (for [[factor expr] psi]
          [factor (get-vars-in-expr expr variables)])))

;; Function to substitute variables in expressions
(defn substitute-in-expr
  "Recursively substitutes variables in expr according to subs, handling chained substitutions."
  [expr subs]
  (cond
    (symbol? expr)
    (let [substituted (get subs expr expr)]
      (if (symbol? substituted)
        (if (= substituted expr)
          substituted
          (substitute-in-expr substituted subs))
        substituted))
    (coll? expr)
    (map #(substitute-in-expr % subs) expr)
    :else expr))

;; Function to identify substitutions
(defn identify-substitutions 
  "Identifies substitution candidates in psi and returns a map with substitutions and factors to remove."
  [psi variables]
  (reduce
    (fn [result [factor expr]]
      (let [[dist arg1 arg2] expr]
        (if (= dist 'dirac)
          (cond
            ;; Case: (dirac var const)
            (and (is-variable? arg1 variables) (is-constant? arg2 variables))
            (-> result
                (update :subs assoc arg1 arg2)
                (update :factors-to-remove conj factor))
            ;; Case: (dirac const var)
            (and (is-constant? arg1 variables) (is-variable? arg2 variables))
            (-> result
                (update :subs assoc arg2 arg1)
                (update :factors-to-remove conj factor))
            ;; Case: (dirac var1 var2)
            (and (is-variable? arg1 variables) (is-variable? arg2 variables))
            (-> result
                (update :subs assoc arg1 arg2)
                (update :factors-to-remove conj factor))
            ;; Otherwise, do nothing
            :else
            result)
          ;; Not a dirac distribution, do nothing 
          result)))
    {:subs {} :factors-to-remove #{}}
    psi))

;; Function to apply substitutions
(defn apply-substitutions 
  "Applies substitutions to all expressions in psi, excluding factors to be removed."
  [psi subs factors-to-remove]
  (into {}
        (for [[factor expr] psi
              :when (not (contains? factors-to-remove factor))]
          [factor (substitute-in-expr expr subs)])))

;; Function to remove substituted elements and update adjacency
(defn remove-substituted-elements 
  "Removes substituted variables and factors from :X, :F, :psi, and updates :A."
  [factor-graph subs factors-to-remove]
  (let [graph (second factor-graph)
        variables (set/difference (set (:X graph)) (set (keys subs)))
        updated-psi (apply dissoc (:psi graph) factors-to-remove)
        updated-F (vec (remove (set factors-to-remove) (:F graph)))
        updated-A (update-adjacency updated-psi variables)
        variables-in-psi (apply set/union (vals updated-A))
        updated-X (vec variables-in-psi)]
    ;; Return the updated graph
    [(first factor-graph)
     (assoc graph
            :X updated-X
            :F updated-F
            :psi updated-psi
            :A updated-A)
     (last factor-graph)]))

;; Main function to perform partial evaluation
(defn remove-cruft 
  "Performs partial evaluation on the factor graph."
  [factor-graph]
  (loop [fg factor-graph]
    (let [graph (second fg)
          variables (set (:X graph))
          psi (:psi graph)
          {:keys [subs factors-to-remove]} (identify-substitutions psi variables)]
      (if (empty? subs)
        fg  ;; No substitutions to perform
        (let [updated-psi (apply-substitutions psi subs factors-to-remove)
              ;; Update variables after substitutions
              variables-after-subst (set/difference variables (set (keys subs)))
              updated-graph [(first fg)
                             (assoc graph :psi updated-psi :X (vec variables-after-subst))
                             (last fg)]]
          ;; Remove substituted elements and recurse in case further substitutions can be made
          (recur (remove-substituted-elements updated-graph subs factors-to-remove)))))))

(comment
  
(def factor-graph-1
  [{}
   {:X ['x 'y 'z],
    :F ['f1 'f2 'f3],
    :A {'x 'f1,
        'y 'f2,
        'z 'f3},
    :psi {'f1 ['normal 'x 0 1],
     'f2 ['normal 'y 0 1],
     'f3 ['dirac 'x 'y]}}
   []])
  
  (def result-1 (remove-cruft factor-graph-1))
  (println result-1)
  
  (def factor-graph-2
    [{}
     {:X ['x 'y 'z],
      :F ['f1 'f2 'f3 'f4 'f5],
      :A {'x 'f1,
          'y 'f2,
          'z 'f3},
      :psi {'f1 ['normal 'x 0 1],
       'f2 ['normal 'y 0 1],
       'f3 ['normal 'z 0 1],
       'f4 ['dirac 'x 'y],
       'f5 ['dirac 'y 'z]}}
     []])
  
  (def result-2 (remove-cruft factor-graph-2))
  (println result-2)
  )