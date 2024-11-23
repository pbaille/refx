(ns refx.registry
  (:require [refx.interop :as interop]
            [refx.log :as log]))

(defn lookup
  ([registry kind id]
   (let [handler (get-in @registry [kind id])]
     (when interop/debug-enabled?
       (when (nil? handler)
         (log/error "no" (str kind) "handler registered for:" id)))
     handler))
  ([registry kind id not-found]
   (get-in @registry [kind id] not-found)))

(defn add! [registry kind id handler]
  (swap! registry assoc-in [kind id] handler)
  handler)

(defn remove! [registry kind id]
  (swap! registry update kind dissoc id)
  nil)

(defn clear! [registry kind]
  (swap! registry dissoc kind)
  nil)

(defn clear-all! [registry]
  (reset! registry {})
  nil)
