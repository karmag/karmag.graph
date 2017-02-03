(ns karmag.graph.query-test
  (:require [clojure.test :refer :all]
            [karmag.graph.core :refer :all]
            [karmag.graph.query :refer :all]))

(let [node-1 (create-node :type :alpha, :num 101)
      node-2 (create-node :type :alpha, :num 202)
      node-3 (create-node :type :omega, :num 101)
      link-1 (create-link :context :hi, :id 10)
      link-2 (create-link :context :lo, :id 11)
      link-3 (-> (create-link :context :lo, :id 12, :viewer :default)
                 (update-view :origin assoc :viewer :origin-specific))]
  (def _graph (-> (create-graph)
                  (add-node node-1)
                  (add-node node-2)
                  (add-node node-3)
                  (add-link link-1 node-1 node-2)
                  (add-link link-2 node-1 node-3)
                  (add-link link-3 node-2 node-3)))
  (def _n1 (get-item _graph node-1))
  (def _n2 (get-item _graph node-2))
  (def _n3 (get-item _graph node-3)))

(deftest chained-test
  (is (= [_n1] (query _graph _n1 [(link :id 10)
                                  (link :id 12)
                                  (link :id 11)]))))

(deftest prop-test
  (is (= #{_n1 _n2} (set (query _graph [(prop :type :alpha)]))))
  (is (= #{_n1}     (set (query _graph [(prop :type :alpha :num 101)]))))
  (is (= #{_n1 _n3} (set (query _graph [(prop :num #(= 101 %))])))))

(deftest link-test
  (is (= #{_n1 _n2} (set (query _graph _n3 [(link)]))))
  (is (= #{_n2}     (set (query _graph _n3 [(link :id 12)]))))
  (is (= #{_n1 _n2} (set (query _graph _n3 [(link :context #(#{:lo} %))])))))

(deftest view-test
  (is (= [_n3] (query _graph _n2 [(link :viewer :origin-specific)])))
  (is (= []    (query _graph _n3 [(link :viewer :origin-specific)])))
  (is (= [_n2] (query _graph _n3 [(link :viewer :default)]))))
