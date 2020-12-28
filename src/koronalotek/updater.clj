(ns koronalotek.updater
  (:require [chime.core :as chime]
            [clojure.string :as string]
            [java-time :as jtime]
            [reaver]))

(defn parse-doc [doc]
  (let [as-of-str (some-> (reaver/select q ".global-stats > p") first .childNodes first str (string/replace "Dane pochodzą z Ministerstwa Zdrowia, aktualne na : " "") .trim)]
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

(defn job [time]
  (println "Called at:" time))

(defn start [{:keys [every]}]
  (chime/chime-at (chime/periodic-seq (jtime/instant) (apply jtime/duration every))
                  job))

(defn stop [scheduler]
  (.close scheduler))
