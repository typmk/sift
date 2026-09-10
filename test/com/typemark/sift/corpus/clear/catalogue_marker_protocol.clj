(ns corpus.clear.catalogue-marker-protocol)
(defprotocol IShape (area [s]) (perimeter [s]))
(defprotocol IStore "A place values go." (put! [s k v]) (fetch [s k]))
