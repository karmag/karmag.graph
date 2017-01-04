(ns karmag.graph.protocol)

;;--------------------------------------------------
;; core protocols

(defprotocol Ident
  (get-id [this]
    "Returns the id of the item or the object itself if it is
    an id."))

(defprotocol Node
  (get-links [this]
    "Returns a sequence of idents pointing to the links that this
    node have."))

(defprotocol Link
  (get-origin [this]
    "Returns the ident of the origin item.")
  (get-target [this]
    "Returns the ident of the target item."))

(defprotocol Graph
  (add-node [this node-fragment]
    "Add the given node to the graph. Returns an updated graph.")
  (add-link [this link-fragment origin-ident target-ident]
    "Add a link between the given nodes. Returns an updated graph.")
  (all-items [this]
    "Returns a sequence of all items contained in this graph.")
  (get-item [this ident]
    "Returns the corresponding item or nil.")
  (remove-item [this ident]
    "Removes the given item. Returns an updated graph.")
  (update-item [this ident f args]
    "Updates the given item by (apply f item args). Returns an
    updated graph."))

;;--------------------------------------------------
;; extended protocols

(defprotocol ItemExt
  (node? [this]
    "Returns truthy value if item is a node.")
  (link? [this]
    "Returns truthy value if item is a link."))

(defprotocol LinkExt
  (other-end [this node-ident]
    "Returns the ident of the node that is not the given ident."))

(defprotocol GraphExt
  (all-nodes [this]
    "Returns all nodes.")
  (all-links [this]
    "Returns all links."))
