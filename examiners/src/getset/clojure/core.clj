(ns ^{:doc "Functions we wouldn't mind seeing in clojure.core (abridged)."}
  getset.clojure.core
  (:require clojure.pprint
            [taoensso.timbre :as log]
            clojure.set
            [clojure.string :as str]
            swiss.arrows
            [clojure.algo.monads :as m]
            [clojure.tools.macro :as macro]))

;;; Forward declarations

(declare transform)


;;; Annoyances

(def str-join str/join)
(def str-split str/split)

;;; Vars

(defn var-root [var]
  (alter-var-root var identity))

;;; Namespaces

(letfn [(move-var [var sym]
          (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
            (if (.hasRoot var)
              (intern *ns* sym (var-root var))
              (intern *ns* sym))))]

  (defn immigrate
    "Create a public var in this namespace for each public var in the
  namespaces named by ns-names. The created vars have the same name, root
  binding, and metadata as the original except that their :ns metadata
  value is this namespace."
    [& ns-names]
    (doseq [ns ns-names]
      (require ns)
      (doseq [[sym ^clojure.lang.Var var] (ns-publics ns)]
        (move-var var sym))))
  
  (defn immigrate-from
    "Like `immigrate`, except wth a list of named symbols."
    [ns symbols]
    (doseq [sym symbols]
      (move-var (ns-resolve ns sym) sym))))

;;; Logging

;; The idea of an "obstinate" log level is one useful for testing.
;; Consider: a clojure app will have a :main entry in its project.clj.
;; In that case, `lein repl` will load the app's code first, then
;; `(use midje.repl)` will load the .midje.clj config file. But
;; when midje is run from `lein midje`, Midje's config file is
;; loaded first, then the app is loaded. Tests want a different
;; level than the app-in-production does. The way to ensure that
;; in both scenarios is for the the test's setting to take precedence.
;;
;; Because it's only used for testing, the mechanism for setting the
;; obstinate level is in `getset.test.logging`.

(defonce #^:dynamic *obstinate-log-level* (atom nil))

(defn log-level []
  (:current-level (deref log/config)))

(defn suggest-log-level!
  "Set a log level but do not override `set-obstinate-log-level`."
  [new-level]
  (when-not (deref *obstinate-log-level*)
    (log/set-level! new-level)))

(suggest-log-level! :info)

(swap! log/config assoc :prefix-fn
       (fn [{:keys [level timestamp hostname ns]}]
         (str timestamp " " (-> level name str/upper-case)
              " [" ns "]")))

(immigrate-from 'taoensso.timbre '[trace debug info warn
                                   tracef debugf infof warnf
                                   with-log-level])

;; The error and fatal macros are adjusted so that they always produce a stack trace.
(defmacro error [arg & args]
  `(let [evaled# ~arg]
     (if (instance? Throwable evaled#)
       (log/error evaled# ~@args)
       (log/error (new Error (str evaled# " " (str-join " " (list ~@args))))))))
(defmacro errorf [arg & args]
  `(let [evaled# ~arg]
     (if (instance? Throwable evaled#)
       (log/error evaled# ~@args)
       (log/error (new Error (format evaled# ~@args))))))
(defmacro fatal [arg & args]
  `(let [evaled# ~arg]
     (if (instance? Throwable evaled#)
       (log/fatal evaled# ~@args)
       (log/fatal (new Error (str evaled# " " (str-join " " (list ~@args))))))))
(defmacro fatalf [arg & args]
  `(let [evaled# ~arg]
     (if (instance? Throwable evaled#)
       (log/fatal evaled# ~@args)
       (log/fatal (new Error (format evaled# ~@args))))))

(defmacro with-throwables-logged-as-errors [& body]
  `(try 
     ~@body
   (catch Throwable ex#
     (log/error ex#)
     (throw ex#))))

(defn wrap-with-logged-throwables [handler]
  (fn [& args]
    (with-throwables-logged-as-errors
      (apply handler args))))


;; Log functions are nested macros. Sometimes it's useful to apply logging functions.
;; Here are some functions to apply. This could be done more elegantly.

(macro/macrolet [(make-log-functions [function-symbol macro-symbol]
                   `(defn ~function-symbol
                      ([a1#] (~macro-symbol a1#))
                      ([a1# a2#] (~macro-symbol a1# a2#))
                      ([a1# a2# a3#] (~macro-symbol a1# a2# a3#))
                      ([a1# a2# a3# a4#] (~macro-symbol a1# a2# a3# a4#))
                      ([a1# a2# a3# a4# a5#] (~macro-symbol a1# a2# a3# a4# a5#))))]

  ;; These are all we use in practice
  (make-log-functions ferror error)
  (make-log-functions ferrorf errorf)
  (make-log-functions fwarn warn)
  (make-log-functions fwarnf warnf))

;;; Maps

;; TODO: Why does this exist, given that update-in also does?
;; Just because it has a default? Can add default with, f'rex, 
;; (update-in kvs :key (fnil conj []))
(defn transform-in
  "Transform the value of the given `keyseq`, using `f`, 
   which is given the original value.

   (let [original {:a {:count 1}, :b ..irrelevant..}]
     (transform original [:a :count] inc) => {:a {:count 2}, :b ..irrelevant..})"
   
  ([map keyseq f default]
     (assoc-in map keyseq
               (f (get-in map keyseq default))))
  ([map keyseq f]
     (transform-in map keyseq f nil)))

(defn transform
  "Transform the value of the given `key`, using `f`, 
   which is given the original value.

   (let [original {:a 1, :b ..irrelevant..}]
     (transform original :a inc) => {:a 2, :b ..irrelevant..})"
   
  ([map key f] (transform-in map [key] f))
  ([map key f default] (transform-in map [key] f default)))

;; TODO: This is a horrible name. The name suggests then and 
;; else clauses, rather than a `pred` and `f` clause.
(defn transform-if 
  "If the value of `key` matches the `pred`, transform
   that value using `f`."
  [map key pred f]
  (if (pred (key map))
    (transform map key f)
    map))

(defn transform-existing
  "If `map` contains `key`, transform using `f`."
  [map key f]
  (if (contains? map key)
    (transform map key f)
    map))

;; TODO: Why is this unlike all the other transform functions in having the map last?
(defn transform-each-value [f map]
  "Transform each value of the `map` by passing it to 
   the given function.
  
   (let [original {:a 1, :b 2}]
     (transform-each-value inc original) => {:a 2, :b 3})"
   
  (apply hash-map (mapcat (fn [[key val]]
                            [key (f val)])
                          map)))

;;; Printing

(immigrate-from 'clojure.pprint '[pprint cl-format])

(defn pprint-str [val] (with-out-str (pprint val)))

;;; Control flow

(defmacro pred-cond 
  "Checks each predicate against the item, returning the corresponding 
   result if it finds a match, otherwise returning nil.
   Assumes item to be a value, as it will get evaluated multiple times."
  [item pred result & preds+results]
  (cond (= pred :else ) result
        (not (seq preds+results)) `(if (~pred ~item) ~result nil) ;; last condition, but no :else in the form
        :else `(if (~pred ~item)
                 ~result
                 (pred-cond ~item ~@preds+results))))

(defmacro when-maybe
  "Like `when-let` except there can be multiple bindings. If any binding value
   is nil, the remainder (plus the body) are not evaluated.

   Note: unlike `when-let`, a `false` value does not short-circuit evaluation."
  [bindings & body]
  `(m/domonad m/maybe-m ~bindings (do ~@body)))

(immigrate-from 'swiss.arrows '[-<> -<>> -!> -!>> -!<> some-<> some-<>>])

(defmacro ?!
  "Use this with some-<>, the nil-shortcutting swiss arrow.
  The incoming value is passed to the first form. Two cases:
  1. The first form returns nil. The incoming value is passed to the
     second form (again using -<> placement), then the entire form
     returns nil (thus short-circuiting the enclosing some-<>).
  2. The first form returns anything else. That becomes the value of
     the whole form.

  Example: 
     (some-<> kvs
              (?! :some-key (errorf \"%s has no key.\" <>))
              second
              (?! extract-short-path (errorf? \"%s should contain a short path\"))
              transform)"
  [earlier calc-form error-form]
  `(let [earlier# ~earlier
         result# (-<> earlier# ~calc-form)]
     (if (nil? result#)
       (do (-<> earlier# ~error-form) nil)
       result#)))

;;; The following macros are useful for terse-ish error reporting in 
;;; arrow flows. Here's an example:
;;; 
;;;   (some-<> event
;;;            (?!e :resume_at_path              "%s does not have a :resume_at_path field")
;;;            (?!e (re-find #"path=(.*?)\&" <>) "%s does not have a `path=` component")
;;;            second
;;;            percent-decode))]
;;;
;;; The format string can have multiple directives within it.

(defmacro ?!log [log-fun [earlier calc-form format-string & other-args]]
  `(?! ~earlier ~calc-form (~log-fun ~format-string ~(symbol "<>") ~@other-args)))

(defn logger-macro-body [function & other-args] `(?!log ~function ~other-args))
(defmacro ?!f [& args] (apply logger-macro-body 'fatalf args))
(defmacro ?!e [& args] (apply logger-macro-body 'errorf args))
(defmacro ?!w [& args] (apply logger-macro-body 'warnf args))
(defmacro ?!i [& args] (apply logger-macro-body 'infof args))
(defmacro ?!d [& args] (apply logger-macro-body 'debugf args))
(defmacro ?!t [& args] (apply logger-macro-body 'tracef args))

;;; Laziness

(defn eagerly
  "Descend form, converting all lazy seqs into lists.
   Metadata is preserved. In the result all non-collections
   are identical? to those in the original form (as is
   their metadata). None of the collections are identical?
   even if they contain no lazy seqs."
  ;; Modified from clojure.walk/walk
  [form]
  (let [m #(with-meta % (meta form))]
    (pred-cond form
      (some-fn seq? list?)    (m (apply list (map eagerly form)))
      vector?                 (m (vec (map eagerly form)))
      map?                    (m (into form (map eagerly form)))
      set?                    (m (into (empty form) (map eagerly form)))
      :else                   form)))

