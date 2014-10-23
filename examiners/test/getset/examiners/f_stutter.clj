(ns getset.examiners.f-stutter (:require [getset.examiners.stutter :as subject])
  (:use midje.sweet getset.clojure.core getset.test.input-maps)
  (:require [getset.examiners.util :as u])
  (:require [clj-time.core :as time]))

(fact "conservative checking of stutters"
  (let [required [:feed_entry_type :feed_entry_id :visibility :likers :comment_ids
                  :signal_created :discussion_changed]
        values ["type" "guid" "private" [] [] (time/now) (time/now)]
        correct (zipmap required values)]
    (fact "requires certain elements"
      (subject/conservative-stutter-examiner correct) => correct
      
      (doseq [omit required]
        (subject/conservative-stutter-examiner (dissoc correct omit)) => u/invalid?))

    (tabular
      (fact "there is type checking"
        (subject/conservative-stutter-examiner (assoc correct ?key ?value)) => u/invalid?)
      ?key                  ?value
      :feed_entry_type      1
      :feed_entry_id        1
      :visibility           "neither private nor public"
      :likers               1
      :comment_ids          1
      :signal_created       "2014-05-07T17:30:47.934Z"
      :discussion_changed   "2014-05-07T17:30:47.934Z")))


(fact "liberal checking of feed entries"
  (let [partial-entry {:feed_entry_type "type"
                       :feed_entry_id "guid"
                       :visibility "private"
                       :likers []
                       :signal_created default-moment}
        without_discussion_changed (assoc partial-entry :comment_ids [])
        without_comment_ids (assoc partial-entry :discussion_changed (time/now))
        valid (merge without_comment_ids without_discussion_changed)]
    
    (fact "will tack on a :discussion_changed"
      (let [actual (subject/liberal-stutter-examiner without_discussion_changed)]
        actual => (contains without_discussion_changed)
        actual => (contains {:discussion_changed default-moment})))
    
    (fact "will tack on an empty :comment_ids"
      (let [actual (subject/liberal-stutter-examiner without_comment_ids)]
        actual => (contains without_comment_ids)
        actual => (contains {:comment_ids []})))

    (fact "converts a map comment id - thanks Lua! - to an empty vector"
      (let [actual (subject/liberal-stutter-examiner (assoc valid :comment_ids {}))]
        actual => (contains valid)
        actual => (contains {:comment_ids []})))

    (fact "does the same for likers"
      (let [actual (subject/liberal-stutter-examiner (assoc valid :likers {}))]
        actual => (contains valid)
        actual => (contains {:likers []})))

    (fact "but also does conservative checks"
      (subject/liberal-stutter-examiner valid) => valid
      (subject/liberal-stutter-examiner (dissoc valid :feed_entry_type)) => u/invalid?)))
