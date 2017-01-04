(ns karmag.graph.core
  (:require [karmag.graph.protocol :as prot]
            [karmag.graph.basic :as basic]))

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
(def other-end prot/other-end)
(def all-nodes prot/all-nodes)
(def all-links prot/all-links)
