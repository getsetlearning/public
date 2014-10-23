(ns getset.f-interfaces (:require [getset.interfaces :as subject])
  (:use midje.sweet getset.clojure.core getset.test.logging)
  (:require [clj-time.core :as time])
  (:require [bouncer.core :as b]
            [getset.validators :as v]))

(facts "about `certify`"
  (fact "normally returns transformed argument"
    (subject/certify {:a :b} :any-map) => {:a :b})
  
  (fact "will return nil if examiner does (for use with some->)"
    (with-transient-log-level :fatal 
      (some-> {}
              (subject/certify :nil-returner)
              (/ 0.0))  ; should not be reached
      => nil)

    (fact "... and it produces an error-level message"
      (log-output :error (subject/certify 56 :any-map))
      => #"The following value fails :any-map: 56"))

  (fact "can also accept `false` for failure (turns into nil)"
    (with-transient-log-level :fatal 
      (subject/certify {} :false-returner) => nil)

    (fact "... and it produces an error-level message"
      (log-output :error (subject/certify {} :false-returner))
      => #"The following value fails :false-returner: \{\}"))

  (fact "bouncer-style verifiers also produce error messages"
    (with-transient-log-level :fatal 
      (subject/certify {} :conservative-stutter) => nil)

    (let [output (log-output :error (subject/certify {} :conservative-stutter))]
      output => #"fails :conservative-stutter: \{}"
      output => #"comment_ids must be present"))

  (fact "an incorrect certificate-name throws an exception"
    (subject/certify {} :some-garbage) => (throws Error #"No map examiner for :some-garbage")))
