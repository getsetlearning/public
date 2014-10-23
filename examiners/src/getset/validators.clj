(ns getset.validators
  "All Bouncer validators, plus some of our own"
  (:use getset.clojure.core)
  (:require [clj-time.core :as time])
  (:require [bouncer.validators :as v])
  (:import org.joda.time.DateTime))

(immigrate 'bouncer.validators)

(v/defvalidator prohibited
  {:default-message-format "%s must not be present."}
  [maybe-present]
  (nil? maybe-present))

(v/defvalidator string
  {:default-message-format "%s must be a string."}
  [value]
  (string? value))

(v/defvalidator object-sequential
  {:default-message-format "%s must be non-string sequential."}
  [value]
  (and (not (string? value))
       (sequential? value)))

(v/defvalidator integer-sequence
  {:default-message-format "%s must be an integer sequence."}
  [value]
  (and (sequential? value)
       (every? integer? value)))

(v/defvalidator integer-id 
  {:default-message-format "%s must be an integer id."}
  [value]
  (and (integer? value)
       (>= value 0)))

(v/defvalidator strict-boolean
  {:default-message-format "%s must be a true boolean."}
  [value]
  (or (= true value) (= false value)))

(v/defvalidator date-time
  {:default-message-format "%s must be a joda DateTime."}
  [value]
  (instance? DateTime value))

(v/defvalidator inclusive-range
  {:default-message-format "%s must be in range."}
  [value lower-bound upper-bound]
  (contains? (set (range lower-bound (inc upper-bound))) value))

(v/defvalidator w3-email 
  "Email validation per http://www.w3.org/TR/html5/forms.html#valid-e-mail-address"
  {:default-message-format "%s is likely to be an invalid email address."}
  [value]
  (and (string? value)
       (boolean (re-matches #"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$" value))))
