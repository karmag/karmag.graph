(ns karmag.graph.visualize
  (:refer-clojure :exclude [print])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [karmag.graph.core :refer :all]))

(defn- indent [text count]
  (let [indent (apply str (repeat count "    "))]
    (->> (s/split-lines text)
         (map #(str indent %))
         (s/join "\n"))))

(defn- collect-items [graph item-data-coll opt]
  (loop [depth (range (:depth opt))
         result []
         [item-data & item-data-coll] item-data-coll
         encountered (set (map :item item-data-coll))]
    (cond
      ;; reached max
      (<= (:max opt)
          (+ (count result) (if item-data 1 0) (count item-data-coll)))
      (concat result (when item-data [item-data]) item-data-coll)
      ;; done
      (empty? depth) (cons item-data item-data-coll)
      ;; depth done
      (not item-data) (recur (rest depth) [] result encountered)
      ;; expand
      (= (:depth item-data) (first depth))
      (let [item (:item item-data)]
        (if (node? item)
          (let [links (->> (get-links item)
                           (map #(get-item graph %))
                           (map #(hash-map :item %
                                           :depth (inc (first depth))
                                           :repeat (boolean (encountered %))
                                           :node item)))]
            (recur depth
                   (apply conj result item-data links)
                   item-data-coll
                   (apply conj encountered (map :item links))))
          (let [raw-nodes (if-let [node (:node item-data)]
                            [(get-item graph (other-end item node))]
                            (map #(get-item graph %)
                                 [(get-origin item)
                                  (get-target item)]))
                nodes (map #(hash-map :item %
                                      :depth (inc (first depth))
                                      :repeat (boolean (encountered %)))
                           raw-nodes)]
            (recur depth
                   (apply conj result item-data nodes)
                   item-data-coll
                   (apply conj encountered (map :item nodes))))))
      ;; else
      :else
      (recur depth
             (conj result item-data)
             item-data-coll
             (conj encountered (:item item-data))))))

(defn- render [item & [other]]
  (with-out-str
    (if (node? item)
      (println "[" (.toString (get-id item)) "]")
      (println "--" (.toString (get-id item)) "-->"
               (or (when other
                     (.toString (get-id other))) ""))) (pprint (strip item))))

(defn- render-repeat [item]
  (if (node? item)
    (str "[" (.toString (get-id item)) "] (repeat)")
    (str "--" (.toString (get-id item)) "--> (repeat)")))

(defn- render-item-coll [item-coll opt]
  (->> item-coll
       (filter #(if (:repeat opt)
                  true
                  (not (:repeat %))))
       (map (fn [item-data]
              (if (:repeat item-data)
                (indent (render-repeat (:item item-data)) (:depth item-data))
                (indent (render (:item item-data)
                                (when (:node item-data)
                                  (other-end (:item item-data) (:node item-data))))
                        (:depth item-data)))))
       (s/join "\n\n")))

(defn items
  "Returns a string representation of the given idents and any items
  linked with those idents. ident-or-coll may be either a single ident
  or a sequential collection of idents.

  Additional options can be given to control the rendering. :depth
  controls the maximum number of links to step through for any given
  chain of items. If :repeat is true items that have already been
  shown will be rendered again in a minimized form if
  encountered. :max specifies the maximum total amout of items to
  render. Defaults are :depth 2, :repeat false, :max 100."
  [graph ident-or-coll & {:keys [depth repeat max] :as opt}]
  (let [opt (merge {:depth 2
                    :repeat false
                    :max 100}
                   opt)
        ident-or-coll (if (and (coll? ident-or-coll)
                               (not (map? ident-or-coll)))
                        ident-or-coll
                        [ident-or-coll])]
    (let [collected (collect-items graph
                                   (->> ident-or-coll
                                        (map #(get-item graph %))
                                        (map #(hash-map :item %
                                                        :depth 0
                                                        :repeat false)))
                                   opt)]
      (render-item-coll collected opt))))

(defn print
  "Calls 'items' and prints the result to *out*."
  [graph ident-or-coll & opts]
  (println (apply items graph ident-or-coll opts)))
