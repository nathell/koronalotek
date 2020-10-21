(ns koronalotek.core
  (:require [clojure.string :as string]
            [hiccup2.core :refer [html]]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as response]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]])
  (:import [java.util Date]))

(defonce state (atom {:date "2020-10-17"
                      :cases 9622
                      :winners []
                      :guesses []}))

(defn winners-count [{:keys [date cases]}]
  (let [month-names ["stycznia" "lutego" "marca" "kwietnia" "maja" "czerwca"
                     "lipca" "sierpnia" "września" "października" "listopada" "grudnia"]
        new-cases (if (and (contains? #{2 3 4} (mod cases 10))
                           (not (contains? #{12 13 14} (mod cases 100))))
                    "nowe przypadki"
                    "nowych przypadków")
        [y m d] (map #(Long/parseLong %) (string/split date #"-"))]
    [:div {:class "winners-count"}
     [:div d " " (month-names (dec m))]
     [:div.number (str cases)]
     [:div new-cases [:br "w Polsce"]]]))

(defn layout [& content]
  (str
   "<!DOCTYPE html>\n"
   (html
    [:html
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:link {:rel "stylesheet" :href "style.css"}]
      [:title "koronalotek"]]
     (into [:body] content)])))

(defn winners [{:keys [winners]}]
  [:section.winners
   [:h2 "dzisiejsi zwycięzcy"]
   (if (seq winners)
     [:ul
      (for [{:keys [name guess delta]} winners]
        [:li
         [:b guess]
         " ("
         (cond (zero? delta) [:b "dokładnie!"]
               (pos? delta) "+"
               (neg? delta) "−")
         (when-not (zero? delta)
           (Math/abs delta))
         ") – "
         name])]
     [:ul
      [:li "to pierwsze losowanie – jeszcze nie było zwycięzców!"]])])

(defn page []
  (layout
   [:section.title
    [:h1 "korona" [:wbr] [:span "lotek"]]]
   [:section.next
    [:h2 "zgaduj zgadula"]
    [:form {:action "/" :method "post"}
     [:label {:for "guess"} "ile przypadków jutro?"]
     [:input {:type "number" :name "guess" :min 0}]
     [:label {:for "name"} "Twoje imię"]
     [:input {:type "text" :name "name"}]
     [:button "zgaduję"]]]
   (winners-count @state)
   (winners @state)))

(defn calculate-winners [cases guesses timestamp]
  (->> guesses
       (filter #(neg? (compare (:timestamp %) timestamp)))
       (map #(assoc % :delta (- (:guess %) cases)))
       (sort-by #(Math/abs (:delta %)))
       (take 10)))

(defn format-date [d]
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") d))

(defn update-state
  [{:keys [cases guesses] :as state} timestamp new-cases]
  {:date (format-date timestamp)
   :cases new-cases
   :guesses []
   :winners (calculate-winners new-cases guesses timestamp)})

(defn new-data!
  [timestamp new-cases]
  (swap! state update-state timestamp new-cases)
  nil)

(defn confirmation [ok?]
  (layout
   [:section.title
    [:h1 "korona" [:wbr] [:span "lotek"]]]
   [:section.next
    [:h2 (if ok? "mamy Twój typ" "coś nie tak")]
    [:p [:a {:href "/"} "wróć na stronę główną"]]]))

(defn validate-guess [{{:strs [guess name] :as params} :form-params, :keys [remote-addr headers]}]
  (try
    (when (and (string? guess) (string? name))
      (let [guess (Long/parseLong guess)
            ip (or (get headers "x-real-ip") remote-addr)]
        (when (>= guess 0)
          {:guess guess, :name name, :timestamp (Date.), :ip ip})))
    (catch Exception _ nil)))

(defn handle-guess [request]
  (if-let [guess (validate-guess request)]
    (do
      (let [new-state (swap! state update :guesses conj guess)]
        (spit "state.edn" (pr-str new-state)))
      (response/response (confirmation true)))
    (response/response (confirmation false))))

(defn basic-handler [{:keys [request-method uri] :as request}]
  (condp = [request-method uri]
    [:get "/"] (response/response (page))
    [:post "/"] (handle-guess request)
    {:status 404,
     :headers {"Content-Type" "text/plain; charset=utf-8"},
     :body "żodyn"}))

(def handler
  (-> basic-handler
      (wrap-params)
      (wrap-resource "/")))

(comment
  (new-data! #inst "2020-10-21T10:30+02:00" 10040)
  (def j (jetty/run-jetty #'handler {:port 8008, :join? false})))
