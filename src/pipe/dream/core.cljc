(ns pipe.dream.core)

(def |> '|>)
(def _> '_>)
(def X> 'X>)
(def X 'X)

(defn handle-op
  [[[op] body]]
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
          (into (list 'X 'as->)))))

(defmacro pipe
  [& body]
  (let [init (first body)]
    (->> body
         (partition-by (partial #{'|> '_> 'X>}))
         rest
         (partition 2)
         (map handle-op)
         (#(-> %
             (conj init)
             (conj '->))))))
