(ns karmag.graph.core-supplement-test
  (:require [clojure.test :refer :all]
            [karmag.graph.core :refer :all]))

(defn- find-node [graph & kvs]
  (let [nodes (query graph [(prop (apply hash-map kvs))])]
    (assert (< (count nodes) 2))
    (first nodes)))

(deftest create-nodes-test
  (let [graph (batch (create-graph) [[:a {:key 1}]
                                     [{:key 2} :b]])]
    (is (find-node graph :key 1))
    (is (find-node graph :key 2))))

(deftest create-links-test
  (testing "link operation ordering"
    (let [graph (batch (create-graph) [[:a {:key 1}]
                                       [:b {:key 2}]
                                       [:a :b {:link 1}]
                                       [:a {:link 2} :b]
                                       [{:link 3} :a :b]])
          a (find-node graph :key 1)
          b (find-node graph :key 2)]
      (doseq [i (range 1 4)]
        (is (= b (first (query graph a [(link {:link i})])))))))
  (testing "referencing existing node"
    (let [graph (batch (create-graph) [[:a {:key 1}]
                                       [:b {:key 2}]])
          a (find-node graph :key 1)
          b (find-node graph :key 2)
          graph (batch graph [[a (get-id b) {:link 1}]])]
      (is (= (find-node graph :key 2)
             (first (query graph a [(link :link 1)]))))))
  (testing "setting view data"
    (let [graph (batch (create-graph)
                       [[:a {:key 1}]
                        [:b {:key 2}]
                        [[:a {:link 10}] {:link 1} [{:link 20} :b]]])
          a (find-node graph :key 1)
          b (find-node graph :key 2)]
      (is (= b (first (query graph a [(link :link 10)]))))
      (is (= a (first (query graph b [(link :link 20)]))))
      (is (= 1 (:link (get-item graph (first (get-links a)))))))))
