(ns koronalotek.time)

(defn now []
  (java.util.Date.))

(defn format-date [d]
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") d))
