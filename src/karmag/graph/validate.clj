(ns karmag.graph.validate
  (:require [karmag.graph.core :refer :all]
            [karmag.graph.walk :refer [walk]]))

;;--------------------------------------------------
;; error

(defrecord ErrorReport [message code id data children])

(defn mk-err
  ([kvs]
   (ErrorReport. (:message kvs)
                 (:code kvs)
                 (get-id (:id kvs))
                 (merge (dissoc kvs :message :code :id :data :children)
                        (:data kvs))
                 (:children kvs)))
  ([k v & kvs]
   (mk-err (apply hash-map k v kvs))))

(defn error-seq [xs]
  (if (instance? ErrorReport xs)
    [xs]
    (mapcat error-seq xs)))

;;--------------------------------------------------
;; validation

(defn view
  "Modifies another validation function by treating the given item as
  a link and merging the view specific data into the item before
  running validation."
  [direction validation-fn]
  (fn [graph item]
    (let [view (merge item (get-view item direction))]
      (map (fn [error]
             (let [error (if (:id error)
                           error
                           (assoc error :id (get-id item)))]
               (update-in error [:data] assoc :view-direction direction)))
           (error-seq (validation-fn graph view))))))

(defn- prepare-constraints [xs]
  (let [xs (if (coll? xs) xs [xs])]
    (reduce (fn [result x]
              (cond
                (keyword? x) (case x
                               :not-nil (assoc result :not-nil true))
                (class? x) (update result :class conj x)
                (ifn? x) (update result :fn conj x)))
            nil
            xs)))

(defn- check-constraint [data item key value]
  (cond
    ;; nil not allowed
    (and (nil? value) (:not-nil data))
    (mk-err :message (str "Key " key " must not be nil")
            :code :missing-mandatory-key
            :key key
            :id item)
    ;; nil value
    (nil? value)
    []
    ;; check constraints
    :else
    (concat (reduce (fn [errors pred]
                      (if (pred value)
                        errors
                        (conj errors
                              (mk-err :message (str "Key " key
                                                    " violates constraint "
                                                    (.getName (class pred)))
                                      :code :constraint-failed
                                      :id item
                                      :constraint pred
                                      :key key))))
                    nil
                    (:fn data))
            (reduce (fn [errors cls]
                      (if (instance? cls value)
                        errors
                        (conj errors
                              (mk-err :message (str "Key " key
                                                    " must be of type "
                                                    (.getName cls))
                                      :code :invalid-type
                                      :class cls
                                      :id item
                                      :class-name (.getName cls)
                                      :key key))))
                    nil
                    (:class data)))))

(defn key-pred
  "Takes key value pairs that map item keys to a number
  of constraints.

  A value/constraint can be any of

      A class - In which case the constraint fails if the item value
      is not of the given class.

      IFn implementation - A unary function that returns a logically
      true value if it is satisfied.

      :not-nil - Asserts that the value is not nil.

      A vector - Which must contain values as per above.

  If the item value is nil validation will not take place unless
  the :not-nil constraint is specified."
  ([kvs]
   (let [data (reduce (fn [m [k v]]
                        (assoc m k (prepare-constraints v)))
                      nil
                      kvs)]
     (fn [graph item]
       (reduce (fn [errors [k data]]
                 (concat errors
                         (->> (check-constraint data item k (get item k))
                              error-seq
                              (map #(assoc % :id (get-id item))))))
               nil
               data))))
  ([k v & kvs]
   (key-pred (apply hash-map k v kvs))))

;;--------------------------------------------------
;; core functionality

(defn set-key-fn
  "Key function is used to find the corresponding validation
  functions. Key fn is (fn [graph item]). The value returned is used
  to lookup the corresponding validation functions. The key function
  can be a keyword."
  [config f]
  (assoc config :key-fn (if (keyword? f)
                          (fn [_ item] (f item))
                          f)))

(defn add-item-fn
  "Validation functions are (fn [graph item]) that return an
  ErrorReport or a sequence of ErrorReports."
  [config key f]
  {:pre [(not (nil? key))]}
  (update-in config [:fns key] #(conj (or % #{}) f)))

(defn validate
  "Performs validation based on the given configuration. Returns a
  sequence of errors, if any."
  [config graph]
  (walk graph
        {:state nil
         :item-fn (fn [state graph item]
                    (let [key ((:key-fn config) graph item)]
                      (reduce (fn [state f]
                                (concat state (error-seq (f graph item))))
                              state
                              (get-in config [:fns key]))))}))
