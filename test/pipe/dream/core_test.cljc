(ns pipe.dream.core-test
  (:require
   #?(:clj [clojure.test
            :refer [deftest is testing]]
      :cljs [cljs.test
             :refer-macros [deftest is testing]])
   #?(:clj  [pipe.dream.core
             :refer [defpipe pipe |> _> X> X >>]]
      :cljs [pipe.dream.core
             :refer [|> _> X> X >>]
             :refer-macros [defpipe pipe]])))



(deftest basic-tests
  (testing "Thread first equivalent"
    (testing "Modify a map"
      (is (= (-> {:x 1}
               (update :x inc)
               (update :x str)
               (assoc :y "3"))
             (pipe
              {:x 1}
              |> update :x inc
              |> update :x str
              |> assoc :y "3")))))

  (testing "Thread last equivalent"
    (testing "Increment 0..10 and add together"
      (is (= (->> (range 10) (map inc) (reduce +))
             (pipe
              (range 10)
              _> map inc
              _> reduce +))))

    (testing "Sum of odd fibonacci numbers under 10,000"
      (is (= 14328
             (pipe
              [0 1]
              _> iterate (fn [[a b]] [b (+ a b)])
              _> map first
              _> take-while (partial > 10000)
              _> filter odd?
              _> reduce +)))))

  (testing "as-> equivalent"
    (testing "Test to see if `hellolleh` is a palidrome"
      (is (true?
           (pipe
            "hellolleh"
            X> = X (apply str (reverse X)))))))

  (testing "kitchen sink"
    (testing "Some random math"
      (is (= 412
             (pipe
              "random math"
              |> seq
              _> map int
              X> into X (rest X)
              |> constantly
              X> list (first (X)) (second (X)) (nth (X) 2)
              _> (fn [[x1 x2 x3]] (pipe x1 |> + x2 |> - x3))
              |> inc
              |> str
              |> seq
              |> cycle
              _> drop 2
              _> take 3
              _> map int
              _> map #(- % 48)
              |> reverse
              _> map vector (iterate (partial * 10) 1)
              |> reverse
              _> map (partial reduce *)
              _> reduce +))))))


(defn- throw-err [msg]
  #?(:clj  (throw (Exception. msg))
     :cljs (throw (js/Error. msg))))

(defn- test-interceptor [result pred]
      (if (pred result)
        result
        (throw-err (str "Failed with result: " result))))

(deftest defpipe-tests
  (testing "Basic defpipe"
    (defpipe test-pipe
      |> assoc :a 1
      |> assoc :b 2
      |> assoc :c 3)

    (is (= {:a 1 :b 2 :c 3}
           (test-pipe {}))))

  (testing "defpipe with interceptor"
    (defpipe interceptor-pipe
      :interceptor test-interceptor
               >> int?
      |> inc   >> (partial = 2)
      |> * 2   >> (partial = 4)
      |> range >> seq?)

    (is (= '(0 1 2 3)
           (interceptor-pipe 1))))

  (testing "defpipe with interceptor that fails"
    (defpipe interceptor-fail-pipe
      :interceptor test-interceptor
      |> str
      |> first >> (partial = \b))

    (is (true? (try
                 (interceptor-fail-pipe (range 97 123))
                 (catch #?(:clj Exception :cljs js/Object) _
                   true)))))

  (testing "defpipe docstring"
    (defpipe pipe-with-doc
      :doc "this is the docstring")
    (is (= "this is the docstring"
           (:doc (meta #'pipe-with-doc)))))

  (testing "defpipe arg"
    (defpipe pipe-with-arg
      :arg (constantly 1)
      |> apply '())

    (is (= 1
           (pipe-with-arg)))

    (testing "defpipe let"
      (defpipe pipe-with-let
        :let {:keys [x y]}
        |> assoc :x+y (+ x y))

      (is (= {:x 1 :y 2 :x+y 3}
             (pipe-with-let {:x 1 :y 2}))))))
