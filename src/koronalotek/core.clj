(ns koronalotek.core
  (:require [clojure.string :as string]
            [hiccup2.core :refer [html]]
            [integrant.core :as integrant]
            [koronalotek.handler :as handler]
            [ring.adapter.jetty :as jetty])
  (:import [java.util Date]))

(def config {:server {:port 8008}})

(defmethod integrant/init-key :server [_ {:keys [port]}]
  (jetty/run-jetty #'handler/handler {:port port, :join? false}))

(defmethod integrant/halt-key! :server [_ server]
  (.stop server))

(defonce system
  (integrant/init config))

(comment
  (new-data! #inst "2020-10-21T10:30+02:00" 10040)
  (def j (jetty/run-jetty #'handler {:port 8008, :join? false})))
