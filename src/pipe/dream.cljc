(ns pipe.dream)

;; PIPE OPERATORS
;;---------------

(def |> '|>)
(def _> '_>)



;; MACRO HELPERS
;;--------------

(defn- handle-op
  "Given parsed pipe syntax, place previous return values in their designated
  places. Since `handle-pipe` uses `->`, `_>` and `X>` must be altered to work
  within a thread-first thread."
  [_ [[op] body]]
  (condp = op
    '|> body
    '_> (-> body
            vec
            (conj 'X)
            seq
            list
            (into (list 'X 'as->)))))


(defn- handle-pipe
  "Parses pipe syntax into a `->`."
  [_ body]
  (let [first-arg (first body)]
    (->> body
         (partition-by (partial #{'|> '_>}))
         rest
         (partition 2)
         (map (partial handle-op nil))
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


(defn handle-multiple-inputs
  "Allows for multi-arity pipes."
  [in]
  (if (> (count in) 1) in (first in)))



;; MACROS
;;-------

(defmacro pipe
  "An alternative to clojure's threading macros. Clojure expressions should be
  unwrapped when passed to this macro. Depending on the operator used, the
  placement of the previous statement's return is passed to the desired location
  of the current statement.

  This namespace provides these valid operators:
  1) `|>` equivalent to `->`.
  2) `_>` equivalent to `->>`.

  Example:
  (pipe {:x 1} |> update :x inc |> assoc :y 3 |> vals _> reduce +)
  ;; => 5"
  [& body]
  (handle-pipe {} body))


(defmacro defpipe
  "Wraps the `pipe` macro in a `defn`, where the argument is passed as the first
  argument to the `pipe` macro.

  Options that can be supplied after the name of the `pipe`:
  1) `:doc` adds a docstring to the function.
  2) `:let` destructure the argument to the pipe.
  3) `:arg` sets the first argument to the pipe."
  [name & args]
  (let [{:keys [arg body doc let]} (opt-map args)
        doc (doc-str doc name)]
    (list 'defn name
          doc
          '[& in]
          (cond

            (and let arg)
            (list 'let [let (list 'pipe.dream/handle-multiple-inputs 'in)]
                  (handle-pipe {} (conj body arg)))

            let
            (list 'let [[let] 'in ['in] 'in]
                  (handle-pipe {} (conj body 'in)))

            arg
            (handle-pipe {} (conj body arg))

            :else
            (list 'let [['in] 'in]
                  (handle-pipe {} (conj body 'in)))))))
