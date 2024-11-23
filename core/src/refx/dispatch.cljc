(ns refx.dispatch
  (:require [refx.interop :refer [after-render empty-queue next-tick]]
            [refx.events :as events]
            [refx.log :as log]))

(def later-fns
  {:flush-dom (fn [f] (after-render #(next-tick f)))
   :yield     next-tick})

(defprotocol IEventQueue
  ;; -- API
  (push [this event])
  (add-post-event-callback [this id callback-fn])
  (remove-post-event-callback [this id])
  (purge [this])

  ;; -- Implementation via a Finite State Machine
  (-fsm-trigger [this trigger arg])

  ;; -- Finite State Machine actions
  (-add-event [this event])
  (-process-1st-event-in-queue [this])
  (-run-next-tick [this])
  (-run-queue [this])
  (-exception [this ex])
  (-pause [this later-fn])
  (-resume [this])
  (-call-post-event-callbacks [this event]))

(deftype EventQueue [^:mutable fsm-state
                     ^:mutable queue
                     ^:mutable post-event-callback-fns
                     event-handler]
  IEventQueue

  (push [this event]
    (-fsm-trigger this :add-event event))

  (add-post-event-callback [_ id callback-fn]
    (when (contains? post-event-callback-fns id)
      (log/warn "overwriting existing post event call back with id:" id))
    (->> (assoc post-event-callback-fns id callback-fn)
         (set! post-event-callback-fns)))

  (remove-post-event-callback [_ id]
    (if-not (contains? post-event-callback-fns id)
      (log/warn "could not remove post event call back with id:" id)
      (->> (dissoc post-event-callback-fns id)
           (set! post-event-callback-fns))))

  (purge [_]
    (set! queue empty-queue))

  (-fsm-trigger
    [this trigger arg]
    (locking this
      (let [[new-fsm-state action-fn]
            (case [fsm-state trigger]
              [:idle :add-event] [:scheduled #(do (-add-event this arg)
                                                  (-run-next-tick this))]

              [:scheduled :add-event] [:scheduled #(-add-event this arg)]
              [:scheduled :run-queue] [:running #(-run-queue this)]

              [:running :add-event]  [:running #(-add-event this arg)]
              [:running :pause]      [:paused #(-pause this arg)]
              [:running :exception]  [:idle #(-exception this arg)]
              [:running :finish-run] (if (empty? queue)
                                       [:idle]
                                       [:scheduled #(-run-next-tick this)])

              [:paused :add-event] [:paused #(-add-event this arg)]
              [:paused :resume]    [:running #(-resume this)]

              (throw (ex-info (str "router state transition not found. " fsm-state " " trigger)
                              {:fsm-state fsm-state, :trigger trigger})))]
        (set! fsm-state new-fsm-state)
        (when action-fn (action-fn)))))

  (-add-event
    [_ event]
    (set! queue (conj queue event)))

  (-process-1st-event-in-queue
    [this]
    (let [event-v (peek queue)]
      (try
        (event-handler event-v)
        (set! queue (pop queue))
        (-call-post-event-callbacks this event-v)
        (catch #?(:cljs :default :clj Exception) ex
          (-fsm-trigger this :exception ex)))))

  (-run-next-tick
    [this]
    (next-tick #(-fsm-trigger this :run-queue nil)))

  (-run-queue
    [this]
    (loop [n (count queue)]
      (if (zero? n)
        (-fsm-trigger this :finish-run nil)
        (if-let [later-fn (some later-fns (-> queue peek meta keys))]
          (-fsm-trigger this :pause later-fn)
          (do (-process-1st-event-in-queue this)
              (recur (dec n)))))))

  (-exception
    [this ex]
    (purge this)
    (throw ex))

  (-pause
    [this later-fn]
    (later-fn #(-fsm-trigger this :resume nil)))

  (-call-post-event-callbacks
    [_ event-v]
    (doseq [callback (vals post-event-callback-fns)]
      (callback event-v queue)))

  (-resume
    [this]
    (-process-1st-event-in-queue this)
    (-run-queue this)))

(defn mk-dispatcher [{:keys [event-handler]}]

  (let [event-queue (->EventQueue :idle empty-queue {} event-handler)]

    {:queue event-queue
     :dispatch (fn dispatch
                 [event]
                 (if (nil? event)
                   (throw (ex-info "you called \"dispatch\" without an event vector." {}))
                   (push event-queue event))
                 nil)

     :dispatch-sync (fn dispatch-sync
                      [event-v]
                      (event-handler event-v)
                      (-call-post-event-callbacks event-queue event-v)
                      nil)}))
