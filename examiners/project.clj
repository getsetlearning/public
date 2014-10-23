(defproject getset "0.13.3"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.taoensso/timbre "2.7.1"]
                 [swiss-arrows "1.0.0"]
                 [bouncer "0.3.1"]]
  :target-path "target"
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}})
