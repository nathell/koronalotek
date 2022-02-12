(ns koronalotek.updater
  (:require [cheshire.core :as json]
            [chime.core :as chime]
            [clojure.string :as string]
            [java-time :as jtime]
            [koronalotek.state :as state]
            [koronalotek.time :as time]
            [reaver]))

(def data-uri
  "https://services-eu1.arcgis.com/zk7YlClTgerl62BY/arcgis/rest/services/global_corona_actual_widok3/FeatureServer/0/query?f=json&cacheHint=true&resultOffset=0&resultRecordCount=1&where=1%3D1&orderByFields=&outFields=*&resultType=standard&returnGeometry=false&spatialRel=esriSpatialRelIntersects")

(defn get-data []
  (try
    (let [data (json/parse-string (slurp data-uri) keyword)
          attributes (-> data :features first :attributes)]
      {:cases (:dzienne_wszystkie_zakazenia attributes),
       :as-of (java.util.Date. (:Data attributes))})))

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
