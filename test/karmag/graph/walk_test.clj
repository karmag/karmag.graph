(ns karmag.graph.walk-test
  (:require [clojure.test :refer :all]
            [karmag.graph.core :refer :all]
            [karmag.graph.walk :refer [walk]]))

(def ^:private graph
  (batch (create-graph)
         [[:a {:key 1}]
          [:b {:key 2}]
          [:c {:key 3}]
          [:a :b {:link 1}]
          [:b :c {:link 2}]]))

(deftest walk-test
  (testing "item fn"
    (is (= (walk graph :item-fn (fn [state graph item]
                                  (conj (or state #{}) (strip item))))
           #{{:key 1} {:key 2} {:key 3} {:link 1} {:link 2}})))
  (testing "node/link fn"
    (is (= (walk graph
                 :node-fn (fn [state graph node]
                            (update state :node #(inc (or % 0))))
                 :link-fn (fn [state graph link]
                            (update state :link #(inc (or % 0)))))
           {:node 3, :link 2}))))
