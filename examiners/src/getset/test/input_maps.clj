(ns getset.test.input-maps
  "Various kinds of maps that flow into programs. Mostly signals."
  (:use getset.clojure.core)
  (:require [getset.interfaces :as iface])
  (:require [clj-time.core :as time]))

(def default-moment (time/date-time 2014))
(def default-moment-string (str default-moment))
(def default-userid 33)
(def default-guid "92e39e5e-fe9d-4e65-b4ba-3d054075e3a2")

;;; Stutters come from signals

(def bare-stutter {:feed_entry_type "reflection"
                   :feed_entry_id default-guid
                   :user_id default-userid
                   :visibility "private"
                   :likers []
                   :comment_ids []
                   :comments []
                   :relevant_wisdom []
                   :relevant_wisdom_ids []
                   :signal_created default-moment
                   :discussion_changed default-moment
                   :facts [{:details ""}]})
  
(defn stutter [& {:as kvs}]
  (-> bare-stutter
      (merge kvs)
      (iface/certify :conservative-stutter)))
