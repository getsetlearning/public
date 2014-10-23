(ns getset.test.logging
  (:use getset.clojure.core)
  (:require [taoensso.timbre :as log]))

(defmacro captured-log-messages [& body]
  `(let [original# @log/config
         accumulator# (atom "")]
     (try
       (log/set-config! [:appenders :standard-out :fn]
                        (fn [maparg#]
                          (let [new-string# (if (:throwable maparg#)
                                              (.getMessage (:throwable maparg#))
                                              (:message maparg#))]
                            (swap! accumulator# str new-string#))))
       (eagerly (do ~@body))
     (finally (reset! log/config original#)))
     (deref accumulator#)))

(defn set-obstinate-log-level! [new-level]
  "Set a log level that can only be overridden by another
   `set-obstinate-log-level`."
  (log/set-level! new-level)
  (reset! *obstinate-log-level* new-level))

(defmacro with-transient-log-level [new-level & body]
  `(let [original# (log-level)]
    (try 
      (binding [*obstinate-log-level* (atom nil)]
        (suggest-log-level! ~new-level)
        (eagerly (do ~@body)))
    (finally 
      (log/set-level! original#)))))

(defmacro log-output [level & body]
  `(captured-log-messages 
    (with-transient-log-level ~level
      (do ~@body))))
  
(def retval (atom nil))

(defmacro log-output-and-result [level & body]
  `(let [log-output# (captured-log-messages 
                      (with-transient-log-level ~level
                        (reset! retval (do ~@body))))]
     [log-output# (deref retval)]))
