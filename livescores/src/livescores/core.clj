(ns livescores.core
  (require [feedparser-clj.core])
  (:use [seesaw core dev border])
  (:require [seesaw.bind :as bind]))


(def leagues {"bundesliga" "http://rss.kicker.de/live/bundesliga" "2. Bundesliga" "http://rss.kicker.de/live/2bundesliga"  "champions-league" "http://rss.kicker.de/live/championsleagu" "DFB-Pokal" "http://rss.kicker.de/live/dfbpokal" "Premier League" "http://rss.kicker.de/live/premierleague" "England Championship" "http://rss.kicker.de/live/thecocacolafootballleaguechampionship" "France" "http://rss.kicker.de/live/thecocacolafootballleaguechampionship" "Spain" "http://rss.kicker.de/live/primeradivision" "Kicker-News" "http://rss.kicker.de/news/aktuell" "Scotland" "http://rss.kicker.de/live/schottland" "Switzerland" "http://rss.kicker.de/live/axposuperleague" "Netherlands" "http://rss.kicker.de/live/eredivisie"
              "sky-news" "http://www.skysports.com/rss/0,20514,11095,00.xml" "soccernews" "http://feeds.feedburner.com/soccernewsfeed"})

(native!)

(defn getleague
  "get feedadress for league"
  [s]
  (get leagues s))

(defn getdesc
  "return parsed description of given feed"
  [league]
  (map #(apply str % "\n\n")
       (map :value (map :description (take 20 (:entries (feedparser-clj.core/parse-feed
                                                   (getleague league))))))))

(defn botdesc
  "return score and description of given feed"
  [league]
  (map #(apply str % "\n\n")
       (interleave (map :title (take 20 (:entries (feedparser-clj.core/parse-feed (getleague league)))))
            (map :value (map :description (take 20 (:entries (feedparser-clj.core/parse-feed
                                                              (getleague league)))))))))


(defn getscore
  "return parsed title of given feed"
  [league]
  (map #(apply str % "\n\n") (map :title (take 20 (:entries (feedparser-clj.core/parse-feed
                                                             (getleague league)))))))

(def text-area 
  "make textarea widget and set textcontent."
  
  ( text
    :maximum-size [600 :by 400]
    :wrap-lines? true
    :font "sans-serif-bold-12"
    :id :te
    :multi-line? true))

(def group (button-group))


(def scorebutton (radio :text "Scores" :group group :selected? true))
(def descbutton (radio :text "Description" :group group))

(def cb (combobox :model (keys leagues)))

(defn updatescores
  "update TextArea with scores of selected league"
  [league]
  (text! text-area (apply str ( getscore league))))

(defn updatedesc
  "update TextArea with description of selected league"
  [league]
  (text! text-area (apply str (botdesc league))))

(defn add-behavior []
  (listen cb :selection (fn [e] (if (= (text (selection group)) "Scores")
                                  (updatescores (selection e))
                                  (updatedesc (selection e)))))
  :scope :self)

(defn make-frame []
  (frame
   :title "Football Scores"
   :size [640 :by 480]
   :content (border-panel
             :north (horizontal-panel
                     :items [cb scorebutton descbutton]
                     :border 4)
             :center (scrollable text-area) 
             :vgap 2
             :hgap 2
             )
   :on-close :exit))


(defn -main [& args]
  (updatedesc "sky-news")
  (add-behavior)
  (invoke-later
   (-> (make-frame)
       pack!
       show!)))


