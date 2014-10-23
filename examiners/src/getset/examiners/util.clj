(ns getset.examiners.util
  (:use getset.clojure.core)
  (:require [bouncer.core :as b]))

;;; Note to readers: the value `::b/errors` is a map of error messages about the rest
;;; of the input map, named `result`.

(defn invalid? [result] 
  (or (::b/errors result) (nil? result) (false? result)))
(def valid? (complement invalid?))


(defn error-string [result]
  (if (::b/errors result) 
    (pprint-str (::b/errors result))
    ""))


(defn emptiness-means-vector
  "Lua has a habit of emitting empty maps into JSON when we need vectors.
   Also converts a missing value (nil) into []"
  [v]
  (if (empty? v) [] v))
