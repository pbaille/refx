(ns refx.subs
  (:require [refx.interop :as interop]
            [refx.registry :as registry]
            [refx.utils :as utils]))

(def kind :sub)

;; --- signals ----------------------------------------------------------------

(defprotocol ISignal
  "Protocol for signal types.

   Very similar to IDeref + IWatchable (e.g. atoms), with the difference that
   listeners will not receive old or new values."
  (-value [this]
    "Returns the current value of the signal.")
  (-add-listener [this k f]
    "Register a listener that will be called without arguments.")
  (-remove-listener [this k]
    "Removes a listener previous registered with `-add-listener`."))

(defn signal? [x]
  (satisfies? ISignal x))

(extend-protocol ISignal
  ;; Useful for missing handlers.  For comparison, re-frame's `subscribe`
  ;; will return `nil`, which is guarenteed to break views that will deref
  ;; subscribed values.
  nil
  (-value [_] nil)
  (-add-listener [_ _ _])
  (-remove-listener [_ _])

  #?(:cljs Atom
     :clj clojure.lang.Atom)
  (-value [a] @a)
  (-add-listener [a k f] (add-watch a k f))
  (-remove-listener [a k] (remove-watch a k)))

;; --- subscription cache -----------------------------------------------------

(defprotocol ICache
  (-cache-lookup [this query-v])
  (-cache-add! [this query-v sub])
  (-cache-remove! [this query-v sub])
  (-cache-dispose! [this sub])
  (-cache-clear! [this])
  (-sub-orphaned [this sub]))

(defprotocol ISub
  (-query-v [this])
  (-orphan? [this])
  (-dispose! [this]))

(extend-protocol ICache
  #?(:cljs Atom
     :clj clojure.lang.Atom)
  (-cache-lookup [this query-v]
    (get @this query-v))

  (-cache-add! [this query-v sub]
    (swap! this assoc query-v sub))

  (-cache-remove! [this query-v sub]
    (swap! this (fn [cache]
                  (if (identical? sub (get cache query-v))
                    (dissoc cache query-v)
                    cache))))
  (-cache-dispose! [this sub]
    (swap! this (fn [cache]
                  (let [query-v (-query-v sub)]
                    (if (identical? sub (get cache query-v))
                      (dissoc cache query-v)
                      cache))))
    (-dispose! sub))
  (-cache-clear! [this]
    (doseq [[_ sub] @this]
      (-cache-dispose! this sub)))
;; TODO This could be changed to support "garbage collection": Don't dispose
;; right away, but keep subscriptions around for a while in case they are
;; requested again.
;; E.g. we could trigger background jobs using window.requestIdleCallback()
  (-sub-orphaned [this sub]
    (interop/next-tick #(when (-orphan? sub)
                          (-cache-dispose! this sub)))))

;; --- subscriptions ----------------------------------------------------------

(deftype Listeners [^:mutable listeners]
  Object
  (empty? [_] (empty? listeners))
  (add [_ k f]
    (set! listeners (assoc listeners k f)))
  (remove [_ k]
    (set! listeners (dissoc listeners k)))
  (notify [_]
    (doseq [[_ f] listeners]
      (f))))

(defn- make-listeners []
  (Listeners. nil))

(defn- map-signals
  "Apply `f` to a node input value."
  [f input]
  (cond
    (signal? input) (f input)
    (sequential? input) (mapv f input) ; run-signal! assumes this is not lazy!
    (map? input) (update-vals input f)
    :else input))

(defn- run-signals! [f input]
  (map-signals f input)
  nil)

(defn- compute-sub [query-v input compute-fn]
  (compute-fn (map-signals -value input) query-v))

(deftype Sub [query-v input compute-fn
              ^:mutable value
              ^:mutable dirty?
              ^Listeners listeners
              cache]

  ISub
  (-query-v [_] query-v)
  (-orphan? [_] (.empty? listeners))
  (-dispose! [this]
    (run-signals! #(-remove-listener % this) input))

  ISignal
  (-value [_] value)
  (-add-listener [_ k f]
    (.add listeners k f))
  (-remove-listener [this k]
    (.remove listeners k)
    (when (.empty? listeners)
      (-sub-orphaned cache this)))

  Object
  (init! [this]
    (let [cb  #(.invalidate! this)]
      (run-signals! #(-add-listener % this cb) input)))

  (invalidate! [this]
    (when-not dirty?
      (set! dirty? true)
      ;; TODO: Do we need invalidate-dirty or just update directly?
      (.update! this)
      #_(interop/next-tick #(.update! this))))

  (update! [_]
    (let [new-value (compute-sub query-v input compute-fn)]
      (set! dirty? false)
      (when (not= value new-value)
        (set! value new-value)
        (.notify listeners))))

  #?@(:cljs
      [IDeref
       (-deref [this] (-value this))

       IEquiv
       (-equiv [this other] (identical? this other))

       IHash
       (-hash [this] (goog/getUid this))]))

(defn- make-sub
  [cache query-v input compute-fn]
  (let [value (compute-sub query-v input compute-fn)
        sub   (->Sub query-v input compute-fn value false (make-listeners) cache)]
    (.init! sub)
    sub))

;; --- register ---------------------------------------------------------------

(defn register
  [{:keys [subscription-cache registry]} query-id input-fn compute-fn]
  (letfn [(handler-fn [query-v]
            (make-sub subscription-cache query-v (input-fn query-v) compute-fn))]
    (registry/add! registry kind query-id handler-fn)))

;; --- dynamic ----------------------------------------------------------------
;;
;; Dynamic subscriptions allow callers to place signals in query vectors:
;; (sub [:dynamic (sub [:param1]) (sub [:param2])])
;;
;; This is not very useful in views, as these should be composed in such a way
;; that child components take parameters for their subscriptions as props.
;;
;; However, it can be useful to create more powerful named subscriptions with
;; `reg-sub`, without needing to change how views are organised.
;;
;; Dynamic subs wrap a special "query-sub" that computes the dynamic query
;; vector, and a mutable "value-sub" that is updated whenever the query sub
;; changes.
(deftype DynamicSub [query-v handler-fn query-sub ^:mutable value-sub
                     ^Listeners listeners
                     cache]
  ISub
  (-query-v [_] query-v)
  (-orphan? [_] (.empty? listeners))
  (-dispose! [this]
    (-remove-listener query-sub this)
    (when value-sub
      (-remove-listener value-sub this)))

  ISignal
  (-value [_]
    (-value value-sub))
  (-add-listener [_ k f]
    (.add listeners k f))
  (-remove-listener [this k]
    (.remove listeners k)
    (when (.empty? listeners)
      (-sub-orphaned cache this)))

  Object
  (init! [this]
    (.update! this)
    (-add-listener query-sub this #(.update! this)))

  (update! [this]
    (let [qv (-value query-sub)]
      (when value-sub
        (-remove-listener value-sub this))
      (set! value-sub (or (-cache-lookup cache qv)
                          (handler-fn qv)))
      (-add-listener value-sub this #(.notify listeners))
      (.notify listeners)))

  #?@(:cljs
      [IEquiv
       (-equiv [this other] (identical? this other))

       IHash
       (-hash [this] (goog/getUid this))

       IDeref
       (-deref [this] (-value this))]))

;; Don't consider nil dynamic, even though it is a valid signal.
(def ^:private some-signal?
  (every-pred some? signal?))

(defn- dynamic?
  "Return true if a query vector contains signals."
  [query-v]
  (some some-signal? query-v))

(defn- dynamic-input
  "Input function for dynamic subscriptions, where the query vector contains
   signals.  Returns a map of vector indexes to signals."
  [query-v]
  (into {}
        (keep-indexed (fn [i x]
                        (when (some-signal? x)
                          [i x])))
        query-v))

(defn- dynamic-compute
  "Computation function for dynamic subscriptions.  Returns a query vector
   where signals have been replaced with their current values.  `input` must
   be a map of vector index to value, as returned by `dynamic-input`."
  [input [_ query-v]]
  (reduce-kv assoc query-v input))

(defn- make-dynamic
  "Make a dynamic subscription."
  [cache query-v handler-fn]
  (let [query-sub (make-sub cache [::query query-v] (dynamic-input query-v) dynamic-compute)
        dynamic   (->DynamicSub query-v handler-fn query-sub nil (make-listeners) cache)]
    (.init! dynamic)
    dynamic))

;; --- sub --------------------------------------------------------------------

(defn mk-subscriber
  "Returns a subscription to `query-v`.

   Callers must make sure that the returned object is eventually used, or it
   will leak memory.  This is designed to construct custom subscriptions in
   handlers, React components should use the `use-sub` hook instead."
  [{:keys [subscription-cache registry]}]
  (letfn [(create-sub [query-v]
            (let [query-id (utils/first-in-vector query-v)]
              ;; Note that nil is a valid signal!
              (when-let [handler-fn (registry/lookup registry kind query-id)]
                (let [sub (if (dynamic? query-v)
                            (make-dynamic subscription-cache query-v handler-fn)
                            (handler-fn query-v))]
                  (-cache-add! subscription-cache query-v sub)
                  sub))))]

    (fn sub
      [query-v]
      (or (-cache-lookup subscription-cache query-v)
          (create-sub query-v)))))
