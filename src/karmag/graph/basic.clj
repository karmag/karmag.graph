(ns karmag.graph.basic
  (:require [karmag.graph.protocol :refer :all])
  (:import java.util.UUID))

(defn create-graph
  "Creates a graph object. Treats a single argument as a map and
  merges it with the graph data. Treats multiple arguments as key
  values that are added to the graph."
  ([] {::meta {:items {}}})
  ([data] (merge data (create-graph)))
  ([k v & kvs] (create-graph (apply hash-map k v kvs))))

(defn create-node
  "Same as create-graph but for nodes."
  ([] {::meta {:id (UUID/randomUUID), :links #{}}})
  ([data] (if (node? data)
            data
            (merge data (create-node))))
  ([k v & kvs] (create-node (apply hash-map k v kvs))))

(defn create-link
  "Same as create-graph but for links."
  ([] {::meta {:id (UUID/randomUUID), :origin nil, :target nil}})
  ([data] (if (link? data)
            data
            (merge data (create-link))))
  ([k v & kvs] (create-link (apply hash-map k v kvs))))

;; core protocols
(extend-type clojure.lang.IPersistentMap
  Ident
  (get-id [this] (get-in this [::meta :id]))
  Node
  (get-links [this] (get-in this [::meta :links]))
  Link
  (get-origin [this] (get-in this [::meta :origin]))
  (get-target [this] (get-in this [::meta :target]))
  Graph
  (add-node [this node-fragment]
    (let [node (create-node node-fragment)]
      (assoc-in this [::meta :items (get-id node)] node)))
  (add-link [this link-fragment origin-ident target-ident]
    (let [o-id (get-id origin-ident)
          t-id (get-id target-ident)
          link (-> (create-link link-fragment)
                   (assoc-in [::meta :origin] o-id)
                   (assoc-in [::meta :target] t-id))
          l-id (get-id link)]
      (update-in this [::meta :items]
                 (fn [items]
                   (-> items
                       (assoc l-id link)
                       (update-in [o-id ::meta :links] conj l-id)
                       (update-in [t-id ::meta :links] conj l-id))))))
  (all-items [this]
    (vals (get-in this [::meta :items])))
  (get-item [this ident]
    (get-in this [::meta :items (get-id ident)]))
  (remove-item [this ident]
    (if-let [item (get-item this ident)]
      (if (node? item)
        (-> (reduce (fn [graph id]
                      (remove-item graph id))
                    this
                    (get-links item))
            (update-in [::meta :items] dissoc (get-id item)))
        (reduce (fn [graph id]
                  (update-in graph [::meta :items id ::meta :links]
                             disj (get-id item)))
                (update-in this [::meta :items] dissoc (get-id ident))
                [(get-origin item) (get-target item)]))
      this))
  (update-item [this ident f args]
    (if-let [item (get-item this ident)]
      (assoc-in this [::meta :items (get-id ident)] (apply f item args))
      this)))

(extend-type UUID
  Ident
  (get-id [this] this))

;; extends protocols
(extend-type clojure.lang.IPersistentMap
  ItemExt
  (node? [this] (get-in this [::meta :links]))
  (link? [this] (find (::meta this) :origin))
  (strip [this] (dissoc this ::meta))
  LinkExt
  (other-end [this ident]
    (if (= (get-id ident) (get-origin this))
      (get-target this)
      (get-origin this)))
  (get-view [this view-direction]
    (get-in this [::meta :view view-direction]))
  (update-view [this view-direction f args]
    (apply update-in this [::meta :view view-direction] f args))
  GraphExt
  (all-nodes [this] (filter node? (all-items this)))
  (all-links [this] (filter link? (all-items this))))
