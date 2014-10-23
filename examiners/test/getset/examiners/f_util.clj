(ns getset.examiners.f-util (:require [getset.examiners.util :as subject])
  (:use midje.sweet getset.clojure.core getset.test.input-maps))

(fact "convert odd values into empty lists"
  (subject/emptiness-means-vector [1]) => [1]
  (subject/emptiness-means-vector []) => []
  (subject/emptiness-means-vector {}) => []
  (subject/emptiness-means-vector nil) => [])

