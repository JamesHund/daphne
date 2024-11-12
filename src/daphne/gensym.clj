(ns daphne.gensym)

(def ^:dynamic *current-variable-name* nil)

(def ^:dynamic *gensyms* (atom {}))

(def ^:dynamic *my-gensym*
  (fn [prefix]
    (let [name (or *current-variable-name* (str prefix))
          count (get @*gensyms* name 0)
          new-count (inc count)]
      (swap! *gensyms* assoc name new-count)
      (symbol (str name (if (> count 0) (str "_" count) ""))))))
