(ns pipe.dream.core-test
  (:require
   #?(:clj  [clojure.test
             :refer [deftest is testing]]
      :cljs [cljs.test
             :refer-macros [deftest is testing]])
   #?(:clj  [pipe.dream.core
             :refer [pipe |> _> X> X]]
      :cljs [pipe.dream.core
             :refer [|> _> X> X]
             :refer-macros [pipe]])))



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
      (is (= true
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
