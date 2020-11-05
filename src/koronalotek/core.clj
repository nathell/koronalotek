(ns koronalotek.core
  (:require [clojure.string :as string]
            [hiccup2.core :refer [html]]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :as response]
            [ring.middleware.content-type :refer [wrap-content-type]]
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
     (into
      [:body
       [:section.title
        [:h1 [:a.faq-link {:href "/faq"} "ℹ️"]
         [:a.korona {:href "/"} "korona"] [:wbr] [:a.lotek {:href "/"} [:span "lotek"]]]]]
      content)])))

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

(defn faq []
  (layout
   [:section.faq
    [:h2 "FAQ"]
    [:dl
     [:dt "co można wygrać?"]
     [:dd "Wieczną sławę i chwałę. Chyba że przyjdzie Sasin i ufunduje coś jeszcze."]
     [:dt "czy można typować więcej niż raz?"]
     [:dd "Można. Głosy oddawane z automatu będą usuwane, podobnie jak za wiele głosów jednej osoby."]
     [:dt "jakie dane są zbierane?"]
     [:dd "Imię (albo cokolwiek, co wpiszesz w polu na imię), zgadnięta liczba przypadków, numer IP, data i czas oddania głosu." [:br] "Strona " [:em "nie"] " używa ciasteczek."]
     [:dt "kiedy wyniki?"]
     [:dd "Jutro. Niedługo po tym, jak Ministerstwo Zdrowia opublikuje oficjalne dane z ostatniej doby – na ogół ok. 10:30. Na razie wyniki wpisywane są ręcznie, więc wyniki mogą się opóźnić. Głosy oddane po publikacji danych przez MZ nie są brane pod uwagę."]
     [:dt "dlaczego nie ma mnie na liście?"]
     [:dd "Nie zgadłoś, zagłosowałoś za późno albo Twój głos nie spodobał się administracji."]
     [:dt "kto za tym stoi?"]
     [:dd "Jak to kto? " [:a {:href "http://danieljanus.pl"} "Rząd światowy."]]
     [:dt "czy nosić maseczkę?"]
     [:dd [:a {:href "https://pws.byu.edu/covid-19-and-masks"} "Tak."]]
     [:dt "nie ma żadnej pandemii!"]
     [:dd "Spadaj, foliarzu."]
     [:p [:a {:href "/"} "wróć na stronę główną"]]]]))

(defn page []
  (layout
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
       (map #(dissoc % :timestamp))
       (distinct)
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

(defn success-img []
  [:section.success
   [:p
    [:a {:href "https://zrzutka.pl/kasa-na-aborcyjny-dream-team"}
     [:img {:src (rand-nth ["furiosa.jpg" "wypierdalac.svg" "wojna.svg"])}]]]
   [:p
    [:a {:href "https://zrzutka.pl/kasa-na-aborcyjny-dream-team"} "wrzuć pieniądz na Aborcyjny Dream Team"]]])

(defn confirmation [ok?]
  (layout
   [:section.next
    [:h2 (if ok? "mamy Twój typ" "coś nie tak")]
    (when ok?
      (success-img))
    [:p [:a {:href "/"} "wróć na stronę główną"]]]))

(defn validate-guess [{{:strs [guess name] :as params} :form-params, :keys [remote-addr headers]}]
  (try
    (when (and (string? guess) (string? name))
      (let [guess (Long/parseLong guess)
            ip (or (get headers "x-real-ip") remote-addr)]
        (when (>= guess 0)
          {:guess guess, :name name, :timestamp (Date.), :ip ip})))
    (catch Exception _ nil)))

(defn html-response [body]
  {:status 200, :headers {"content-type" "text/html; charset=utf-8"}, :body body})

(defn handle-guess [request]
  (if-let [guess (validate-guess request)]
    (do
      (let [new-state (swap! state update :guesses conj guess)]
        (spit "state.edn" (pr-str new-state)))
      (html-response (confirmation true)))
    (html-response (confirmation false))))

(defn basic-handler [{:keys [request-method uri] :as request}]
  (condp = [request-method uri]
    [:get "/"] (html-response (page))
    [:get "/faq"] (html-response (faq))
    [:post "/"] (handle-guess request)
    {:status 404,
     :headers {"Content-Type" "text/plain; charset=utf-8"},
     :body "żodyn"}))

(def handler
  (-> basic-handler
      (wrap-params)
      (wrap-resource "/")
      (wrap-content-type)))

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

(comment
  (new-data! #inst "2020-10-21T10:30+02:00" 10040)
  (def j (jetty/run-jetty #'handler {:port 8008, :join? false})))
