(ns koronalotek.core
  (:require [clojure.string :as string]
            [hiccup2.core :refer [html]]
            [integrant.core :as integrant]
            [koronalotek.handler :as handler]
            [koronalotek.updater :as updater]
            [ring.adapter.jetty :as jetty])
  (:import [java.util Date]))

(def config {:server {:port 8008}
             :updater {:every [5 :minutes]}})

(defmethod integrant/init-key :server [_ {:keys [port]}]
  (jetty/run-jetty #'handler/handler {:port port, :join? false}))

(defmethod integrant/halt-key! :server [_ server]
  (.stop server))

(defmethod integrant/init-key :updater [_ args]
  (updater/start args))

(defmethod integrant/halt-key! :updater [_ updater]
  (updater/stop updater))

(defonce system
  (integrant/init config))
