(ns refx.alpha
  (:require #?(:cljs [refx.hooks :as hooks])
            [refx.cofx :as cofx]
            [refx.dispatch :as dispatch]
            [refx.effects :as effects]
            [refx.events :as events]
            [refx.interceptor :as interceptor]
            [refx.interceptors :as interceptors]
            [refx.registry :as registry]
            [refx.db :refer [app-db]]
            [refx.subs :as subs]
            [refx.utils :as utils]))

(defprotocol IFrame

  ;; --- dispatch ---------------------------------------------------------------

  (dispatch [this event])

  (dispatch-sync [this event])

  ;; --- events -----------------------------------------------------------------

  ;; TODO: Provide a registry of interceptors as well, so they can be referenced by ID.

  (reg-event [this id interceptors handler-interceptor])

  (clear-event [this] [this id])

  ;; --- subscriptions ----------------------------------------------------------

  (sub [this query-v])

  (reg-sub
    [this query-id compute-fn]
    [this query-id input-fn compute-fn])

  (clear-sub [this] [this id])

  (clear-subscription-cache! [this])

  ;; --- effects ----------------------------------------------------------------

  (reg-fx [this id handler])

  (clear-fx [this] [this id])

  ;; --- coeffects --------------------------------------------------------------

  (reg-cofx [this id handler])

  (clear-cofx [this] [this id])

  (inject-cofx [this id] [this id value]))

(defrecord Frame [registry app-db subscription-cache subscription-handler dispatcher interceptors]

  IFrame

  ;; --- dispatch ---------------------------------------------------------------

  (dispatch
    [this event]
    ((:dispatch dispatcher) event))

  (dispatch-sync
    [this event]
    ((:dispatch-sync dispatcher) event))

  ;; --- events -----------------------------------------------------------------

  ;; TODO: Provide a registry of interceptors as well, so they can be referenced by ID.

  (reg-event [this id extra-interceptors handler-interceptor]
    (events/register registry id (conj interceptors extra-interceptors handler-interceptor)))

  (clear-event
    [this]
    (registry/clear! registry events/kind))

  (clear-event
    [this id]
    (registry/remove! registry events/kind id))

  ;; --- subscriptions ----------------------------------------------------------

  (sub
    [this query-v]
    (subscription-handler this query-v))

  (reg-sub [this query-id compute-fn]
    (subs/register this query-id (constantly app-db) compute-fn))
  (reg-sub [this query-id input-fn compute-fn]
    (subs/register this query-id input-fn compute-fn))

  (clear-sub [this]
    (registry/clear! registry subs/kind))
  (clear-sub [this id]
    (registry/remove! registry subs/kind id))

  (clear-subscription-cache! [this]
    (subs/-cache-clear! subscription-cache))

  ;; --- effects ----------------------------------------------------------------

  (reg-fx
    [this id handler]
    (effects/register this id handler))

  (clear-fx [this]
    (registry/clear! registry effects/kind))
  (clear-fx [this id]
    (registry/remove! registry effects/kind id))

  ;; --- coeffects --------------------------------------------------------------

  (reg-cofx
    [this id handler]
    (cofx/register this id handler))

  (clear-cofx [this]
    (registry/clear! registry  cofx/kind))

  (clear-cofx [this id]
    (registry/remove! registry cofx/kind id))

  (inject-cofx [this id]
    (cofx/inject-cofx this id))

  (inject-cofx [this id value]
    (cofx/inject-cofx this id value)))

(defn ->interceptor
  [& {:as m}]
  (utils/apply-kw interceptor/->interceptor m))

(defn reg-event-db
  ([frame id handler]
   (reg-event-db id nil handler))
  ([frame id interceptors handler]
   (reg-event frame id interceptors (events/db-handler->interceptor handler))))

(defn reg-event-fx
  ([frame id handler]
   (reg-event-fx id nil handler))
  ([frame id interceptors handler]
   (reg-event frame id interceptors (events/fx-handler->interceptor handler))))

(defn reg-event-ctx
  ([frame id handler]
   (reg-event-ctx id nil handler))
  ([frame id interceptors handler]
   (reg-event frame id interceptors (events/ctx-handler->interceptor handler))))

#?(:cljs (defn subscription-hook [this]
           (hooks/get-subscription-hook this)))

(defn new-frame []
  (let [reg (atom {})
        db (atom {})
        subscription-cache (atom {})
        subscription-handler (subs/mk-subscriber {:registry reg :subscription-cache subscription-cache})
        event-handler (events/mk-handler reg)
        dispatcher (dispatch/mk-dispatcher {:event-handler event-handler})
        do-fx (effects/init {:app-db db :dispatcher dispatcher :registry reg})
        inject-db (cofx/init {:registry reg})]
    ;;[registry app-db subscription-cache subscription-handler dispatcher interceptors]
    (->Frame reg db subscription-cache subscription-handler dispatcher [do-fx inject-db])))
