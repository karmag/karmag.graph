(ns karmag.graph.core-test
  (:require [clojure.test :refer :all]
            [karmag.graph.core :refer :all])
  (:import java.util.UUID))

(let [node-1 (create-node)
      node-2 (create-node)
      link-1 (create-link)]
  (def _graph (-> (create-graph)
                  (add-node node-1)
                  (add-node node-2)
                  (add-link link-1 node-1 node-2)))
  (def _n1 (get-item _graph node-1))
  (def _n2 (get-item _graph node-2))
  (def _l1 (get-item _graph link-1)))

(deftest create-objects-test
  (let [user-data {:key :value}]
    (doseq [obj [(create-graph user-data)
                 (create-node user-data)
                 (create-link user-data)]]
      (is (= :value (:key obj)))
      (is (map? (:karmag.graph.basic/meta obj))))))

(deftest get-id-test
  (is (not (nil? (get-id (create-node)))))
  (is (not (nil? (get-id (create-link))))))

(deftest get-links-test
  (is (= #{(get-id _l1)} (get-links _n1)))
  (is (= #{(get-id _l1)} (get-links _n2))))

(deftest get-endpoints-test
  (is (= (get-origin _l1) (get-id _n1)))
  (is (= (get-target _l1) (get-id _n2))))

(deftest all-*-tests
  (is (= #{_n1 _n2 _l1} (set (all-items _graph))))
  (is (= #{_n1 _n2} (set (all-nodes _graph))))
  (is (= #{_l1} (set (all-links _graph)))))

(deftest remove-item-test
  (testing "remove node"
    (let [graph (remove-item _graph _n1)]
      (is (= [(get-id _n2)] (map get-id (all-items graph))))
      (is (-> graph (get-item (get-id _n2)) get-links empty?))))
  (testing "remove link"
    (let [graph (remove-item _graph _l1)]
      (is (= (set [(get-id _n1) (get-id _n2)])
             (set (map get-id (all-items graph)))))
      (is (-> graph (get-item (get-id _n1)) get-links empty?))
      (is (-> graph (get-item (get-id _n2)) get-links empty?))))
  (testing "remove non-existing"
    (is (= _graph (remove-item _graph (UUID/randomUUID))))))

(deftest update-item-test
  (testing "update node"
    (let [graph (update-item _graph _n1 assoc :key :value)
          node (get-item graph _n1)]
      (is (= :value (:key node)))))
  (testing "update link"
    (let [graph (update-item _graph _l1 assoc :key :value)
          node (get-item graph _l1)]
      (is (= :value (:key node)))))
  (testing "update non-existing"
    (is (= _graph (update-item _graph (UUID/randomUUID) assoc :key :value)))))

(deftest node?-link?-test
  (are [item n? l?] (and (= n? (if (node? item) true false))
                         (= l? (if (link? item) true false)))
       _graph false false
       _n1    true  false
       _n2    true  false
       _l1    false true))
