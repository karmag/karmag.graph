(ns karmag.graph.query
  (:require [karmag.graph.core :refer :all]
            [karmag.graph.protocol :refer [Ident]]))

(def ^:dynamic *graph*)

;;--------------------------------------------------
;; helpers

(defn- matches [mapped item]
  (every? (fn [[k v]]
            (if (fn? v)
              (v (get item k))
              (= v (get item k))))
          mapped))

(defn- query* [graph items directives]
  (binding [*graph* graph]
    (reduce (fn [items dir]
              (doall (distinct (dir items))))
            items
            directives)))

;;--------------------------------------------------
;; interface

(defn query
  "Queries the graph by applying directives. Directives are functions
  that take and returns a sequence of nodes. During evaluation of a
  directive *graph* is bound to the given graph.

  Items are a sequence of items or a single ident. If given it is used
  as the first sequence passed to directives. If not given all nodes
  in the graph is used as starting point instead."
  ([graph directives]
   (query* graph (all-nodes graph) directives))
  ([graph items directives]
   (if (satisfies? Ident items)
     (query* graph [(get-item graph items)] directives)
     (query* graph items directives))))

(defn prop
  "Filters items based on key-value matches. If the value is a
  function, as recognized by fn?, that function is applied and a
  logically true result indicates match."
  ([mapped]
   (fn [items]
     (filter #(matches mapped %) items)))
  ([key value & kvs]
   (prop (apply hash-map key value kvs))))

(defn link
  "Returns nodes on the other side of links. Given no args all linked
  nodes are returned. With arguments the filtering is applied to the
  links in the same manner as for 'prop'."
  ([]
   (fn [items]
     (mapcat (fn [node]
               (->> (get-links node)
                    (map #(get-item *graph* %))
                    (map #(other-end % node))
                    (map #(get-item *graph* %))))
             items)))
  ([mapped]
   (fn [items]
     (mapcat (fn [node]
               (->> (get-links node)
                    (map #(get-item *graph* %))
                    (filter #(matches mapped %))
                    (map #(other-end % node))
                    (map #(get-item *graph* %))))
             items)))
  ([key value & kvs]
   (link (apply hash-map key value kvs))))
