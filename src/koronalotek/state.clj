(ns koronalotek.state
  (:require [koronalotek.time :as time]))

(defonce state (atom {:date "2020-10-17"
                      :cases 9622
                      :winners []
                      :guesses []}))

(defn calculate-winners [cases guesses timestamp]
  (->> guesses
       (filter #(neg? (compare (:timestamp %) timestamp)))
       (map #(dissoc % :timestamp))
       (distinct)
       (map #(assoc % :delta (- (:guess %) cases)))
       (sort-by #(Math/abs (:delta %)))
       (take 10)))

(defn update-state
  [{:keys [cases guesses] :as state} timestamp new-cases]
  {:date (time/format-date timestamp)
   :cases new-cases
   :guesses []
   :winners (calculate-winners new-cases guesses timestamp)})

(defn new-data!
  [timestamp new-cases]
  (swap! state update-state timestamp new-cases)
  nil)

;; admin

(defn remove-ip
  [state ip]
  (update state :guesses (fn [guesses] (remove #(= (:ip %) ip) guesses))))

(defn remove-ip!
  [ip]
  (swap! state remove-ip ip))

(defn ip-freq
  ([] (ip-freq @state))
  ([state]
   (->> state :guesses (map :ip) frequencies (sort-by val >))))

(defn top-names
  ([] (top-names @state))
  ([state]
   (->> (:guesses state) (map :name) (distinct) (sort-by count >))))

(defn count-guesses
  ([] (count-guesses @state))
  ([state] (count (:guesses state))))
