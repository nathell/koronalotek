(ns koronalotek.state
  (:require [clojure.edn :as edn]
            [koronalotek.time :as time]))

(defn load-state
  ([] (load-state "state.edn"))
  ([filename]
   (try
     (edn/read-string (slurp filename))
     (catch Exception _ nil))))

(defonce state (atom (or (load-state)
                         {:date "2020-10-17"
                          :cases 9622
                          :winners []
                          :guesses []})))

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
  (let [old-state @state
        {:keys [date]} (swap! state update-state timestamp new-cases)
        filename (format "state-%s.edn" date)]
    (spit filename (pr-str old-state))
    nil))

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
