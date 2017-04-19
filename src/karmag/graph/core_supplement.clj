(ns karmag.graph.core-supplement
  (:require [karmag.graph.basic :refer :all]
            [karmag.graph.protocol :refer :all]))

(defn- apply-node [data a b]
  (let [[id props] (if (map? a) [b a] [a b])
        node (create-node props)]
    (-> data
        (update-in [:graph] add-node node)
        (update-in [:id-mapping] assoc id (get-id node)))))

(defn- apply-link [data args]
  (loop [args args, origin nil, target nil, props nil, view-data nil]
    (let [arg (first args)]
      (cond
        ;; done
        (empty? args)
        (let [link (-> (create-link props)
                       (update-view :origin merge (:origin view-data))
                       (update-view :target merge (:target view-data)))]
          (update-in data [:graph] add-link link origin target))
        ;; view endpoint
        (and (vector? arg) (= 2 (count arg)))
        (let [[a b] arg
              [id view] (cond
                          (and (satisfies? Ident a) (get-id a))
                          [a b]
                          (and (satisfies? Ident b) (get-id b))
                          [b a]
                          (map? a)
                          [b a]
                          :else
                          [a b])
              view (if origin
                     (assoc view-data :target view)
                     (assoc view-data :origin view))]
          (recur (cons id (rest args)) origin target props view))
        ;; ident
        (and (satisfies? Ident arg) (get-id arg))
        (if origin
          (recur (next args) origin arg props view-data)
          (recur (next args) arg target props view-data))
        ;; props
        (map? arg)
        (recur (next args) origin target arg view-data)
        ;; node id
        :else
        (let [node-id (get-in data [:id-mapping arg])]
          (when-not node-id
            (throw (ex-info (str "No node mapping found for " arg)
                            {:mapping (:id-mapping data), :id arg})))
          (if origin
            (recur (next args) origin node-id props view-data)
            (recur (next args) node-id target props view-data)))))))

(defn- apply-op [data op]
  (cond
    ;; node op
    (= 2 (count op))
    (apply apply-node data op)
    ;; link op
    (= 3 (count op))
    (apply-link data op)
    ;; unknown
    :else
    (throw (ex-info (str "Unknown batch operation " op) {:operation op}))))

(defn batch
  "Execute a number of operations towards the graph using a compact
  format. ops is a sequence of operations. Available operations are:

  Existing nodes can be referenced by passing an ident as id. Ids
  should otherwise not be maps. Maps as arguments are used to
  distinguish nodes and links. The arguments may be in any order.

  Create node [id, node-fragment].

    Creates a node using the given fragment. The id may be used to
    reference the node in next-coming operations.

  Create link [endpoint, endpoint, link-fragment]. Endpoint is single
  id or [id, view-fragment].

    Creates a link from the first encountered endpoint to the second
    endpoint. If the endpoint is specified using the second form the
    corresponding view will be populated with that fragment.

  Example:

  (batch graph [[:node-1 {:key 1}]
                [{:key 2} :node-2]
                [:node-1 [:node-2 {:view :value}] {:link :value}]])"
  [graph ops]
  (let [data {:graph graph
              :id-mapping {}}]
    (:graph (reduce apply-op data ops))))
