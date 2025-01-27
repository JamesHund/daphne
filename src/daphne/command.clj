(ns daphne.command
  (:refer-clojure :exclude [compile])
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.walk :as walk]
            [daphne.address-transformation :refer [address-trafo]]
            [daphne.core :refer [program->graph]]
            [daphne.desugar :refer [desugar]]
            [daphne.desugar-datastructures :refer [desugar-datastructures]]
            [daphne.desugar-hoppl :refer [desugar-hoppl-global]]
            [daphne.factor-graph :refer [graph->factor-graph
                                         reformat-factor-graph remove-cruft
                                         source-code-transformation]]
            [daphne.gensym :refer [*gensyms*]]
            [daphne.hmc :refer [hmc]]
            [daphne.hoppl-cps :refer [hoppl-cps]]
            [daphne.hy :refer [foppl->python]]
            [daphne.metropolis-within-gibbs :refer [metropolis-within-gibbs]]
            [clojure.stacktrace :as stacktrace])
  (:import [java.io PushbackReader StringReader])
  (:gen-class))

;; This file is following https://github.com/clojure/tools.cli

(defn usage [options-summary]
  (->> ["This is the daphne probabilistic program compiler."
        ""
        "Usage: daphne [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  graph                   Create graphical model of the program"
        "  factor-graph            Create graphical model of the program with factors"
        "  factor-transform        Apply preprocessing to source code for factor graph"
        "  desugar                 Return a desugared FOPPL syntax object of the program"
        "  desugar-hoppl           Return a desugared HOPPL syntax object of the program"
        "  desugar-hoppl-noaddress Return a desugared HOPPL syntax object of the program without addresses"
        "  desugar-hoppl-cps       Return a desugared HOPPL syntax object in continuation passing style"
        "  python-class  Create a Python class with sample and log probability methods for the program"
        "  infer         Run inference on the program"
        ""
        "Notes:"
        "  - If no input file or source code is provided, the program reads from STDIN."
        "  - If no output file is specified, the output is written to STDOUT."
        "Please refer to the manual page for more information."]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(def actions #{"graph" "factor-graph" "factor-transform" "desugar" "desugar-hoppl" "desugar-hoppl-noaddress" "desugar-hoppl-cps"
              "python-class" "infer"})

(def cli-options
  ;; An option with a required argument
  (let [formats #{:json :edn :pretty-json}
        algorithms #{:hmc :metropolis-within-gibbs}]
    [["-f" "--format FORMAT" "Output format"
      :default :pretty-json
      :parse-fn keyword
      :validate [formats (str "Must be one of: " (str/join ", " formats))]]
     ["-s" "--source SOURCECODE" "Program source code, by default STDIN is used."
      :default nil]
      ;;:validate [read-string "Cannot read program code."]]
     ["-d" "--daemon" "Run the compiler as a daemon (server mode)"]
     ["-i" "--input-file SOURCEFILE" "Program source file, by default STDIN is used."
      :default nil
      :validate [#(.exists (io/file %))
                 "Program source file does not exist."]]
     [nil "--num-samples N" "Number of samples to draw"
      :default 1000
      :parse-fn #(Integer/parseInt %)
      :validate [pos? "Number of samples must be positive."]]
     ["-a" "--algorithm ALGORITHM" "Inference algorithm to use"
      :default :metropolis-within-gibbs
      :parse-fn keyword
      :validate [algorithms (str "Algorithm must be one of: "
                                 (str/join ", " algorithms))]]
     ["-o" "--output-file OUTPUTFILE"
      "File to write the output to, otherwise STDOUT is used."
      :default nil
      :validate [#(io/file %) "Output file is not a valid file name."]]
     ;; A non-idempotent option (:default is applied first)
     ["-v" nil "Verbosity level"
      :id :verbosity
      :default 0
      :update-fn inc]
     ;; A boolean option defaulting to nil
     ["-h" "--help"]]))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      (:daemon options)
      {:daemon true :options options}

      (and (:source options) (:input-file options))
      {:exit-message "The --source and --input-file options are exclusive."}

      ;; Allow piping input when no source is specified
      (and (empty? (:source options))
           (empty? (:input-file options))
           (empty? arguments))
      {:exit-message "No action specified. Please provide an action."}

      (and (<= 1 (count arguments))
           (actions (first arguments)))
      {:action (keyword (first arguments)) :options options}

      (not (actions (first arguments)))
      {:exit-message (str "Unknown command, must be one of: "
                          (str/join ", " actions))}

      :else
      {:exit-message (usage summary)})))


(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn read-all-exps [s]
  (try
    (with-open [in (PushbackReader. (StringReader. s))]
      (let [edn-seq (repeatedly (partial edn/read {:eof :theend} in))]
        (apply list (take-while (partial not= :theend) edn-seq))))
    (catch Exception e
      (throw (ex-info "Cannot read symbolic expressions from string."
                      {:string s
                       :exception e})))))

(defn infer [code opts]
  (let [algo ({:metropolis-within-gibbs metropolis-within-gibbs
               :hmc                     hmc} (:algorithm opts))]
    (->> code
       algo
       (take (:num-samples opts))
       vec)))

(defn- transform-vals [f m]
  (->> m
       (map (fn [[k v]] [k (f v)]))
       (into {})))

(defn- desugar-datastructures-graph [prog]
  (let [[rho g return] prog]
    [(transform-vals (comp desugar-datastructures desugar) rho)
     (update-in g [:P] (partial transform-vals desugar-datastructures))
     (desugar-datastructures return)]))

(defn execute [action code opts]
  (when (pos? (:verbosity opts))
    (println "Executing" action " for:")
    (apply println code))
    (reset! *gensyms* {})
    (case action
    :graph (-> code program->graph desugar-datastructures-graph)
    :factor-graph (-> code source-code-transformation program->graph desugar-datastructures-graph graph->factor-graph reformat-factor-graph remove-cruft)
    :factor-transform (-> code source-code-transformation program->graph desugar-datastructures-graph graph->factor-graph)
    :desugar (-> code desugar desugar-datastructures)
    :desugar-hoppl (list
                    'fn ['alpha]
                    (-> code desugar-hoppl-global (address-trafo 'alpha)))
    :desugar-hoppl-cps (list
                        'fn ['alpha 'k-return]
                        (-> code desugar-hoppl-global (address-trafo 'alpha) (hoppl-cps 'k-return)))

    :desugar-hoppl-noaddress (-> code desugar-hoppl-global)
    :python-class (foppl->python code)
    :infer (infer code opts)))

(defn add-string-encoding [x]
  (cond ;(symbol? x)  (str "'" (name x))
        ;(keyword? x) (str ":" (name x))
        (string? x)  (str "\"" x "\"")
        :else        x))

(defn run-daemon [options]
  (let [port 6677]
    (println (str "Daphne compiler daemon started on port " port))
    (with-open [server-socket (java.net.ServerSocket. port)]
      (loop []
        (let [client-socket (.accept server-socket)]
          (future
            (with-open [reader (io/reader client-socket)
                        writer (io/writer client-socket)]
              (loop []
                (let [line (try
                             (.readLine reader)
                             (catch java.io.EOFException e
                               ;; Client disconnected
                               nil))]
                  (if (nil? line)
                    ;; Client has closed the connection
                    (println "Client disconnected")
                    ;; Process the request
                    (do
                      (try
                        (let [request-json (json/read-str line :key-fn keyword)
                              {:keys [action code options]} request-json
                              default-options {:verbosity 0}
                              options (-> (merge default-options options)
                                          (update :format keyword))
                              ;; Process the code
                              code-exprs (read-all-exps code)
                              result (execute (keyword action) code-exprs options)
                              response {:status "success" :result result}]
                          ;; Send response back to client
                          (json/write response writer)
                          (.write writer "\n") ; Add newline delimiter
                          (.flush writer))
                        (catch Exception e
                          ;; Handle exceptions during processing
                          (println "Exception occurred while processing request:")
                          (println (.getMessage e))
                          (stacktrace/print-stack-trace e)
                          ;; Send error response to client
                          (let [response {:status "error" :message (.getMessage e)}]
                            (json/write response writer)
                            (.write writer "\n") ; Add newline delimiter
                            (.flush writer))))
                      ;; Continue to process the next request
                      (recur)))))))
          ;; Continue to accept new connections
          (recur))))))


(defn -main [& args]
  (let [{:keys [action options daemon exit-message ok?]} (validate-args args)]
    (cond
      exit-message
      (exit (if ok? 0 1) exit-message)

      daemon
      (run-daemon options)

      :else
      (let [source (or (:source options)
                       (slurp (or (:input-file options) *in*)))
            code (read-all-exps source)
            out' (execute action code options)
            out' (walk/postwalk add-string-encoding out')
            out (if (not (string? out'))
                  (case (:format options)
                    :json (json/write-str out')
                    :pretty-json (with-out-str (json/pprint out'))
                    :edn  (pr-str out'))
                  out')]
        (when (pos? (:verbosity options))
          (println))
        (if (:output-file options)
          (spit (:output-file options) out)
          (println out))
        (System/exit 0)))))

(comment
  (-main "factor-transform" "-i" "programs/test_programs/skills.daphne" "-o" "out.json")
  (-main "factor-graph" "-i" "programs/test_programs/skills.daphne" "-o" "out.json")
  (-main "graph" "-i" "programs/homework2/1.daphne" "-o" "out.json")
  )

