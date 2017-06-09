(ns karmag.graph.validate-test
  (:require [clojure.test :refer :all]
            [karmag.graph.core :refer :all]
            [karmag.graph.validate :as v]))

(deftest view-test
  (let [graph (batch (create-graph)
                     [[:a {:key 1}]
                      [:b {:key 2}]
                      [[:a       {:data :orig, :origin true}]
                       {:type :l, :data :base}
                       [:b       {:data :targ, :target true}]]])]
    (testing "no validation errors"
      (let [config (-> nil
                       (v/set-key-fn :type)
                       (v/add-item-fn :l (v/key-pred :data :not-nil))
                       (v/add-item-fn :l (v/view :origin
                                                 (v/key-pred :data :not-nil
                                                             :origin :not-nil)))
                       (v/add-item-fn :l (v/view :target
                                                 (v/key-pred :data :not-nil
                                                             :target :not-nil))))
            errors (v/validate config graph)]
        (is (zero? (count errors)))))
    (testing "view error"
      (let [fail-config (-> nil
                            (v/set-key-fn :type)
                            (v/add-item-fn :l (v/view :origin
                                                      (v/key-pred :view-name :not-nil))))
            errors (v/validate fail-config graph)
            err (first errors)]
        (is (= 1 (count errors)))
        (is (= :missing-mandatory-key (:code err)))
        (is (= :origin (-> err :data :view-direction)))
        (is (= :view-name (-> err :data :key)))))))

(deftest key-pred-test
  (let [graph (batch (create-graph)
                     [[:a {:type :n :text "hello" :number 101}]])
        item-id (->> (all-items graph) first get-id)
        check (fn [& args]
                (v/validate
                 (v/add-item-fn (v/set-key-fn nil :type)
                                :n
                                (apply v/key-pred args))
                 graph))]
    (testing ":not-nil"
      (testing "pass"
        (is (empty? (check :text :not-nil))))
      (testing "fail"
        (let [errors (check :xyz :not-nil)]
          (is (= 1 (count errors)))
          (are [path value] (= (get-in (first errors) path) value)
            [:message] "Key :xyz must not be nil"
            [:code] :missing-mandatory-key
            [:id] item-id
            [:data :key] :xyz))))
    (testing "class"
      (testing "pass"
        (is (empty? (check :text String, :number Number)))
        (is (empty? (check :xyz String))))
      (testing "fail"
        (let [errors (check :text Integer)]
          (is (= 1 (count errors)))
          (are [path value] (= (get-in (first errors) path) value)
            [:message] "Key :text must be of type java.lang.Integer"
            [:code] :invalid-type
            [:id] item-id
            [:data :key] :text
            [:data :class] Integer
            [:data :class-name] (.getName Integer)))))
    (testing "pred"
      (testing "pass"
        (is (empty? (check :text string? :number integer?)))
        (is (empty? (check :xyz string?))))
      (testing "fail"
        (let [errors (check :text integer?)]
          (is (= 1 (count errors)))
          (are [path value] (= (get-in (first errors) path) value)
            [:message] "Key :text violates constraint clojure.core$integer_QMARK_"
            [:code] :constraint-failed
            [:id] item-id
            [:data :constraint] integer?
            [:data :key] :text))))
    (testing "combination"
      (testing "pass"
        (is (empty? (check :text [:not-nil not-empty String])))
        (is (empty? (check :xyz [not-empty String]))))
      (testing "fail"
        (let [errors (check :text [string? integer? Number])]
          (is (= 2 (count errors)))
          (is (= #{:invalid-type :constraint-failed}
                 (set (map :code errors))))))
      (testing "multiple failures"
        (let [errors (check :text [integer? Number]
                            :number [string? String])]
          (is (= 4 (count errors))))))))
