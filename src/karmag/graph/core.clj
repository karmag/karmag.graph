(ns karmag.graph.core
  (:require [karmag.graph.core-supplement :as supplement]
            [karmag.graph.basic :as basic]
            [karmag.graph.protocol :as prot]
            [karmag.graph.query :as query]))

;; creation
(def create-graph basic/create-graph)
(def create-node basic/create-node)
(def create-link basic/create-link)

;; core protocols
(def get-id prot/get-id)
(def get-links prot/get-links)
(def get-origin prot/get-origin)
(def get-target prot/get-target)
(def add-node prot/add-node)
(def add-link prot/add-link)
(def all-items prot/all-items)
(def get-item prot/get-item)
(def remove-item prot/remove-item)
(def update-item (fn [graph ident f & args]
                   (prot/update-item graph ident f args)))

;; extended protocols
(def node? prot/node?)
(def link? prot/link?)
(def strip prot/strip)
(def other-end prot/other-end)
(def get-view prot/get-view)
(def update-view (fn [link view-direction f & args]
                   (prot/update-view link view-direction f args)))
(def all-nodes prot/all-nodes)
(def all-links prot/all-links)

;; query
(def query query/query)
(def prop query/prop)
(def link query/link)

;; supplement
(def batch supplement/batch)
