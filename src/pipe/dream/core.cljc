(ns pipe.dream.core)



;; PIPE OPERATORS
;;---------------

(def |> '|>)
(def _> '_>)
(def X> 'X>)
(def X 'X)
(def >> '>>)



;; MACRO HELPERS
;;--------------

(defn- handle-op
  "Given parsed pipe syntax, place previous return values in their designated
  places. Since `handle-pipe` uses `->`, `_>` and `X>` must be altered to work
  within a thread-first thread."
  [{:keys [interceptor]} [[op] body]]
  (condp = op
    '|> body
    '_> (-> body
            vec
            (conj 'X)
            seq
            list
            (into (list 'X 'as->)))
    'X> (-> body
            seq
            list
            (into (list 'X 'as->)))
    '>> (if interceptor
          (conj body interceptor)
          (let [err "Found `>>` when an interceptor wasn't provided."]
            #?(:clj  (throw (Exception. err))
               :cljs (throw (js/Error. err)))))))


(defn- handle-pipe
  "Parses pipe syntax into a `->`."
  [{:keys [interceptor]} body]
  (let [first-arg (first body)]
    (->> body
         (partition-by (partial #{'|> '_> 'X> '>>}))
         rest
         (partition 2)
         (map (partial handle-op {:interceptor interceptor}))
         (#(-> %
               (conj first-arg)
               (conj '->))))))


(defn- opt-map
  "Parses the options supplied to the `defpipe` macro."
  [args]
  (let [opts (->> args
                  (partition 2)
                  (reduce (fn [acc [k v]]
                            (if (keyword? k)
                              (assoc acc k v)
                              (reduced acc))) {}))
        body (-> opts count (* 2) (drop args))]
    (assoc opts :body body)))


(defn- doc-str
  "Adds a doc string to the `defpipe` macro."
  [doc name]
  (if doc
    doc
    (str name " does not have a docstring. Add one with `:doc`.")))



;; MACROS
;;-------

(defmacro pipe
  "An alternative to clojure's threading macros. Clojure expressions should be
  unwrapped when passed to this macro. Depending on the operator used, the
  placement of the previous statement's return is passed to the desired index
  of the current statement.

  This namespace provides these valid operators:
  1) `|>` equivalent to `->`
  2) `_>` equivalent to `->>`
  3) `X>` like `as->` where the value is bound to `X`

  Example:
  (pipe {:x 1} |> update :x inc |> assoc :y 3 |> vals _> reduce +)
  ;; => 5"
  [& body]
  (handle-pipe {} body))



(defmacro defpipe
  "Wraps the `pipe` macro in a `defn`, where the argument is passed as the first
  argument to the `pipe` macro.

  There are two options that should be supplied after the name of the `pipe`:
  1) `:doc` adds a docstring to the function
  2) `:interceptor` function that allows the use of the `>>` operator

  The `>>` operator calls the function supplied to the interceptor, where the
  first parameter is the result of the previous statement. Any items between the
  `>>` operator and the next operator (or end of the pipe) will be passed to the
  interceptor function. Ideally, this can be used to spec results and handle
  errors."
  [name & args]
  (let [{:keys [body doc interceptor]} (opt-map args)
        doc (doc-str doc name)]
    (list 'defn name
          doc
          '[in]
          (handle-pipe {:interceptor interceptor} (conj body 'in)))))
