(ns getset.f-validators 
  (:use midje.sweet getset.clojure.core)
  (:require [clj-time.core :as time])
  (:require [bouncer.core :as b] 
            [getset.validators :as v]))

;;;; Custom validators

(defchecker valid [actual] (empty? (first actual)))

(facts "prohibited"
  (fact "succeeds if key is missing"
    (b/validate {} :key v/prohibited) => valid)

  (fact "a `nil` key counts as missing"
    (b/validate {} :key v/prohibited) => valid)

  (fact "errors are produced as normal"
    (b/validate {:key 1} :key v/prohibited)
    => [{:key ["key must not be present."]}
        {:key 1, ::b/errors {:key ["key must not be present."]}}]))


(facts "string"
  (fact "succeeds if key is a string"
    (b/validate {:key "foo"} :key v/string) => valid)

  (fact "errors are produced as normal"
    (b/validate {:key 1} :key v/string)
    => [{:key ["key must be a string."]}
        {:key 1, ::b/errors {:key ["key must be a string."]}}]))

(facts "object-sequential"
  (fact "succeeds if key is sequential"
    (b/validate {:key []} :key v/object-sequential) => valid
    (b/validate {:key '()} :key v/object-sequential) => valid)

  (fact "strings are NOT object-sequential"
    (b/validate {:key "foo"} :key v/object-sequential)
    => [{:key ["key must be non-string sequential."]}
        {:key "foo", ::b/errors {:key ["key must be non-string sequential."]}}])

  (fact "errors are produced as normal"
    (b/validate {:key 1} :key v/object-sequential)
    => [{:key ["key must be non-string sequential."]}
        {:key 1, ::b/errors {:key ["key must be non-string sequential."]}}]))

(fact "integer-id"
  (b/validate {:key 1} :key v/integer-id) => valid
  (b/validate {:key 0} :key v/integer-id) => valid
  (b/validate {:key -1} :key v/integer-id)
  => [{:key ["key must be an integer id."]}
      {:key -1, ::b/errors {:key ["key must be an integer id."]}}]

  (b/validate {:key 2.0} :key v/integer-id)
  => [{:key ["key must be an integer id."]}
      {:key 2.0, ::b/errors {:key ["key must be an integer id."]}}]

  (b/validate {:key "1"} :key v/integer-id)
  => [{:key ["key must be an integer id."]}
      {:key "1", ::b/errors {:key ["key must be an integer id."]}}])

(facts "integer-sequence"
  (b/validate {:key [1 2 3]} :key v/integer-sequence) => valid

  (b/validate {:key "string"} :key v/integer-sequence)
  => [{:key ["key must be an integer sequence."]}
        {:key "string", ::b/errors {:key ["key must be an integer sequence."]}}]

  (b/validate {:key 1} :key v/integer-sequence)
  => [{:key ["key must be an integer sequence."]}
        {:key 1, ::b/errors {:key ["key must be an integer sequence."]}}]

  (b/validate {:key [1 true]} :key v/integer-sequence)
  => [{:key ["key must be an integer sequence."]}
        {:key [1 true], ::b/errors {:key ["key must be an integer sequence."]}}])

(facts "strict boolean"
  (fact "succeeds if key is boolean"
    (b/validate {:key true} :key v/strict-boolean) => valid
    (b/validate {:key false} :key v/strict-boolean) => valid)

  (fact "errors are produced as normal"
    (b/validate {:key 1} :key v/strict-boolean)
    => [{:key ["key must be a true boolean."]}
        {:key 1, ::b/errors {:key ["key must be a true boolean."]}}]

    (b/validate {:key 1} :key v/strict-boolean)
    => [{:key ["key must be a true boolean."]}
        {:key 1, ::b/errors {:key ["key must be a true boolean."]}}]))

(facts "date-time"
  (fact "succeeds if key is a joda-time date time"
    (b/validate {:key (time/now)} :key v/date-time) => valid)

  (fact "errors are produced as normal"
    (b/validate {:key 1} :key v/date-time)
    => [{:key ["key must be a joda DateTime."]}
        {:key 1, ::b/errors {:key ["key must be a joda DateTime."]}}]))
  

(facts "w3-email"
  ;; The ones commented out show cases where the W3 regexp doesn't match RFC 5322
  ;; Test strings mostly from https://fightingforalostcause.net/content/misc/2006/compare-email-regex.php
  (b/validate {:email :not-string} :email v/w3-email) =not=> valid
  (b/validate {:email ""} :email v/w3-email) =not=> valid
  (b/validate {} :email v/w3-email) =not=> valid
  (b/validate {:email "first.last@iana.org"} :email v/w3-email) => valid
  (b/validate {:email "FRED@IANA.ORG"} :email v/w3-email) => valid
  (b/validate {:email "first.last@x23456789012345678901234567890123456789012345678901234567890123.iana.org"} :email v/w3-email) => valid
  (b/validate {:email "first.last@3com.com"} :email v/w3-email) => valid
  (b/validate {:email "user+mailbox@iana.org"} :email v/w3-email) => valid
  (b/validate {:email "customer/department=shipping@iana.org"} :email v/w3-email) => valid
  (b/validate {:email "_somename@iana.org"} :email v/w3-email) => valid
  ; (b/validate {:email "\"test.test\"@iana.org"} :email v/w3-email) => valid
  (b/validate {:email "first..last@iana.org"} :email v/w3-email) => valid
  (b/validate {:email "shaitan@my-domain.thisisminekthx"} :email v/w3-email) => valid
  (b/validate {:email "first.last@com"} :email v/w3-email) => valid
  (b/validate {:email "first.last@example.123"} :email v/w3-email) => valid
  (b/validate {:email "1123@iana.org"} :email v/w3-email) => valid
  
  (b/validate {:email "first.last@sub.do,com"} :email v/w3-email) =not=> valid
  (b/validate {:email "first.last"} :email v/w3-email) =not=> valid
  ; (b/validate {:email ".first.last@iana.org"} :email v/w3-email) =not=> valid
  (b/validate {:email "first.last@"} :email v/w3-email) =not=> valid
  (b/validate {:email "first.last@-xample.com"} :email v/w3-email) =not=> valid
  (b/validate {:email "first.last@exampl-.com"} :email v/w3-email) =not=> valid
  ; (b/validate {:email "two..dot@iana.org"} :email v/w3-email) =not=> valid
  (b/validate {:email "hello world@iana.org"} :email v/w3-email) =not=> valid
  (b/validate {:email "est@@iana.org"} :email v/w3-email) =not=> valid
  (b/validate {:email "test@example."} :email v/w3-email) =not=> valid
  (b/validate {:email "test@.org"} :email v/w3-email) =not=> valid
  (b/validate {:email "NotAnEmail.com"} :email v/w3-email) =not=> valid
)
