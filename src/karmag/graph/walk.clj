(ns karmag.graph.walk
  (:require [karmag.graph.core :refer :all]))

(defn walk
  "Walks all items of the given graph collecting and returning the result.

  Node, link and item functions are (fn [state graph item]) functions
  that return a modified state. If nil is returned the previous state
  will be used as return value instead. If item function is given it
  will be used as default for both nodes and links. If either node or
  link function is nil, and no item function is given, the
  corresponding item type will not be iterated.

  An initial state may be passed which will be nil otherwise."
  ([graph {:keys [node-fn link-fn item-fn state]}]
   (let [node-fn (or node-fn item-fn)
         link-fn (or link-fn item-fn)]
     (let [state (if (nil? node-fn)
                   state
                   (reduce (fn [state node]
                             (or (node-fn state graph node) state))
                           state
                           (all-nodes graph)))]
       (if (nil? link-fn)
         state
         (reduce (fn [state link]
                   (or (link-fn state graph link) state))
                 state
                 (all-links graph))))))
  ([graph k v & kvs]
   (walk graph (apply hash-map k v kvs))))
