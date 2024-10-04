(ns daphne.factor-graph 
  (:require [daphne.desugar-let :refer [desugar-let]]))

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
  (println exp)
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
  (println "Application: " exp)
  (let [[f & args] exp
        _ (println "Function: " f)
        _ (println "Args: " args)
        transformed-f (transform f)
        transformed-args (map (fn [a] (transform a)) args)]
    (println "Transformed function: " transformed-f)
    (println "Transformed args: " transformed-args)
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
        unobserved-vars (clojure.set/difference V (set observed-vars))  ;; Get unobserved variables
        factors (into {} (map (fn [v] [v (symbol (str "f-" (name v)))]) V))  ;; Map factors with corresponding variable names
        factor-nodes (vals factors)                 ;; List of factor nodes (values of factors map)
        variable-nodes unobserved-vars              ;; Variable nodes are the unobserved variables
        edges (into {}                               ;; Create edges mapping
                    (map (fn [[v v-obs]]
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

(defn analyze-factors [psi observed-vars]
   (let [remove-factors (atom #{})  ;; Set of factors to remove
         substitutions (atom {})]    ;; Map to track substitutions

    ;; Analyze each factor
     (doseq [[f exp] psi]
       (let [vars (filter (fn [e] (not (number? e))) exp)  ;; Get variables from the expression
             [v c] (when (= (first exp) "p_dirac") (rest exp))  ;; Extract potential values
             v1 (first vars)
             v2 (second vars)]
         (cond
          ;; Rule 1: Handle forms like (p_dirac v c) or (p_dirac c v)
           (and v (contains? observed-vars v))  ;; If v is observed, remove and substitute
           (do
             (swap! remove-factors conj f)        ;; Mark factor for removal
             (swap! substitutions assoc v c))     ;; Substitute v := c in other factors

          ;; Rule 2: Handle form (p_dirac v1 v2) where both are variables
           (and v1 v2)  ;; Both are variables
           (do
             (swap! substitutions assoc v1 v2)  ;; Substitute v1 := v2
             (swap! remove-factors conj f)))))  ;; Mark factor for removal

    ;; Return the state of remove-factors and substitutions
   {:remove-factors @remove-factors
    :substitutions @substitutions}))

(defn clean-factor-graph [factor-graph]
  (let [{:keys [X F A psi]} (second factor-graph) ;; Extract the graph details
        observed-vars (set (keys (filter (fn [[k v]] (= (first v) "observe*")) psi))) ;; Identify observed vars
        {:keys [remove-factors substitutions]} (analyze-factors psi observed-vars)] ;; Analyze factors

    ;; Helper function to substitute variables in psi expressions
    (letfn [(substitute-vars [expr]
              (cond
                (list? expr) (map substitute-vars expr)
                (contains? substitutions expr) (get substitutions expr)
                :else expr))]

      ;; Construct cleaned factor graph
      (let [cleaned-psi (apply dissoc psi remove-factors) ;; Remove marked factors
            cleaned-X (remove #(contains? substitutions %) X) ;; Remove spurious variables
            cleaned-A (into {} (filter (fn [[k v]] (not (contains? remove-factors v))) A)) ;; Clean edges
            cleaned-F (remove #(contains? remove-factors %) F)] ;; Remove factors

        ;; Apply substitutions to cleaned psi
        (let [final-psi (into {}
                              (map (fn [[k v]]
                                     [k (substitute-vars v)]) cleaned-psi))]

          ;; Return the cleaned factor graph
          [(first factor-graph)
           {:X cleaned-X
            :F cleaned-F
            :A cleaned-A
            :psi final-psi}
           (last factor-graph)])))))