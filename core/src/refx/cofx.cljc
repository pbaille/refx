(ns refx.cofx
  (:require [refx.interceptor :refer [->interceptor]]
            [refx.log :as log]
            [refx.registry :as registry]
            [refx.db :refer [app-db]]))

(def kind :cofx)

(defn register
  [registry id handler]
  (registry/add! registry kind id handler))

;; -- Interceptor -------------------------------------------------------------

(defn inject-cofx
  ([registry id]
   (->interceptor
    :id      :coeffects
    :before  (fn coeffects-before
               [context]
               (if-let [handler (registry/lookup registry kind id)]
                 (update context :coeffects handler)
                 (log/error "No cofx handler registered for" id)))))
  ([registry id value]
   (->interceptor
    :id     :coeffects
    :before  (fn coeffects-before
               [context]
               (if-let [handler (registry/lookup registry kind id)]
                 (update context :coeffects handler value)
                 (log/error "No cofx handler registered for" id))))))

;; -- Builtin CoEffects Handlers  ---------------------------------------------

;; :db
;;
;; Adds to coeffects the value in `app-db`, under the key `:db`
(defn init [{:keys [registry app-db]}]
  (register
   registry
   :db
   (fn db-coeffects-handler
     [coeffects]
     (assoc coeffects :db @app-db)))
  ;; TODO this should be returned as value
  (inject-cofx registry :db))
