(ns karmag.graph.visualize-test
  (:require [clojure.test :refer :all]
            [karmag.graph.core :refer :all]
            [karmag.graph.visualize :as visualize]))

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
  (def _n3 (get-item _graph node-3))
  (def _l1 (get-item _graph link-1)))

(deftest render-sanity-test
  (are [parts length opts]
    (and (= length
            (count
             (apply visualize/items _graph parts opts)))
         (= (inc length)
            (count
             (with-out-str
               (apply visualize/print _graph parts opts)))))
    _n1        457 []
    _n1       1632 [:repeat true :depth 5]
    [_n1 _n2]  572 []
    _l1        477 []))
