(ns getset.examiners.stutter
  (:use getset.clojure.core)
  (:require [bouncer.core :as b]
            [getset.validators :as v]
            [getset.examiners.util :as u]))


(v/defvalidator visibility-string
  {:default-message-format "%s must be `private` or `public`."}
  [value]
  (or (= value "public") (= value "private")))

;;; Note to readers: Bouncer's b/validate function returns a sequence of two results:
;;; 1. a map (or nil). In the map, each key has an error message as its value. Keys
;;;    without errors are omitted.
;;; 2. the original input map with a special key, `bouncer.core/errors`, added. It
;;;    contains the above error map. 
;;; We use the (2) map.

(defn conservative-stutter-examiner [kvs]
  (-> kvs
      (b/validate :signal_created [v/required v/date-time]
                  :discussion_changed [v/required v/date-time]
                  :feed_entry_type [v/required v/string]
                  :feed_entry_id [v/required v/string]
                  :visibility [v/required visibility-string]
                  :comment_ids [v/required v/object-sequential]
                  :likers [v/required v/object-sequential])
      second))

(defn liberal-stutter-examiner [kvs]
  (let [discussion_changed (or (:discussion_changed kvs) (:signal_created kvs))
        comment_ids (u/emptiness-means-vector (:comment_ids kvs))
        likers (u/emptiness-means-vector (:likers kvs))]
    (-> kvs
        (assoc :discussion_changed discussion_changed)
        (assoc :comment_ids comment_ids)
        (assoc :likers likers)
        conservative-stutter-examiner)))
