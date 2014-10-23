(ns getset.interfaces
  "Filtering and erroring for different maps passed across boundaries,
   especially app boundaries"
  (:use getset.clojure.core)
  (:require [clj-time.core :as time])
  (:require getset.examiners.stutter
            [getset.examiners.util :as u])
  (:require [bouncer.core :as b]
            [getset.validators :as v]))


(def examiners {})

(defmacro add-examiners [namespace & certification-keywords]
  `(do (require '~namespace)
       (doseq [kw# '~certification-keywords]
         (alter-var-root #'examiners
                         #(assoc % kw# (var-get (find-var (symbol (name '~namespace)
                                                                  (str (name kw#) "-examiner")))))))))          

;; TODO: This is a pretty clunky way of adding new examiners to `certify`.
(add-examiners getset.examiners.stutter :conservative-stutter :liberal-stutter)

;; Add on some test examiners
(alter-var-root #'examiners (fn [old] (merge old
                                             {:false-returner (constantly false)
                                              :nil-returner (constantly nil)
                                              :any-map #(when (map? %) %)})))

(defn- shouted-reporting [candidate result certificate-name]
  (if (u/invalid? result)
    (error (format "The following value fails %s: %s %s" certificate-name candidate
                   (u/error-string result)))
    result))

(defn certify [candidate certificate-name]
  (if-let [examiner (get examiners certificate-name)]
    (shouted-reporting candidate (examiner candidate) certificate-name)
    (throw (new Error (str "No map examiner for " certificate-name)))))

