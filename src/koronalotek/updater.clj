(ns koronalotek.updater
  (:require [chime.core :as chime]
            [clojure.string :as string]
            [java-time :as jtime]
            [koronalotek.state :as state]
            [koronalotek.time :as time]
            [reaver]))

(defn parse-doc [doc]
  (let [as-of-str (some-> doc (reaver/select ".global-stats > p") first .childNodes first str (string/replace "Dane pochodzą z Ministerstwa Zdrowia, aktualne na : " "") .trim)]
    {:csv-url (some-> doc (reaver/select ".file-download") second (reaver/attr :href))
     :as-of (-> (jtime/local-date-time "dd.MM.yyyy HH:mm" as-of-str)
                (.atZone (jtime/zone-id "Europe/Warsaw"))
                (jtime/to-java-date))}))

(defn fetch-and-extract-csv [{:keys [csv-url] :as data}]
  (let [csv (slurp csv-url)
        rows (string/split csv #"\r\n")
        fields (map #(string/split % #";") rows)]
    (merge data
           {:cases (some-> fields second second Long/parseLong)})))

(defn get-data []
  (try
    (-> (slurp "https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2")
        (reaver/parse)
        (parse-doc)
        (fetch-and-extract-csv))
    (catch Exception e
      (println "Fetch failed:" (.getMessage e))
      nil)))

(def state (atom {:as-of #inst "2020-12-28T09:00:00.000-00:00"
                  :cases 3211}))

(defn job [now]
  (let [previous-state @state
        previous-as-of (:as-of previous-state)
        diff (.getSeconds (jtime/duration previous-as-of now))]
    (if (< diff 86400)
      (println "Skipping update, less than a day passed since previous results")
      (when-let [{:keys [as-of cases] :as new-data} (get-data)]
        (if (and (jtime/before? (jtime/instant previous-as-of) (jtime/instant as-of))
                 (not= (time/format-date as-of) (:date @state/state)))
          (do
            (println "New data fetched:" new-data)
            (reset! state new-data)
            (state/new-data! as-of cases))
          (println "Fetched stale data:" new-data))))))

(defn start [{:keys [every]}]
  (chime/chime-at (chime/periodic-seq (jtime/instant) (apply jtime/duration every))
                  job))

(defn stop [scheduler]
  (.close scheduler))
