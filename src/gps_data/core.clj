(ns gps-data.core
  (:require
    [clj-time.core :as t])
  (:import
    (java.util.regex Pattern)
    (org.joda.time DateTime)))

(def trekpoint-regex (Pattern/compile "<trkpt(.+?)>(.+?)</trkpt>" Pattern/DOTALL))
(def latitude-regex (Pattern/compile "lat=\"(.+?)\"" Pattern/DOTALL))
(def longitude-regex (Pattern/compile "lon=\"(.+?)\"" Pattern/DOTALL))
(def elevation-regex (Pattern/compile "<ele>(.+?)</ele>" Pattern/DOTALL))
(def time-regex (Pattern/compile "<time>(.+?)</time>" Pattern/DOTALL))

(defn- parse-trekpoint [header body]
  (let [[_ latitude-str] (re-find latitude-regex header)
        [_ longitude-str] (re-find longitude-regex header)
        [_ elevation-str] (re-find elevation-regex body)
        [_ time-str] (re-find time-regex body)]
    {:latitude (Double/parseDouble latitude-str)
     :longitude (Double/parseDouble longitude-str)
     :elevation (Double/parseDouble elevation-str)
     :time (DateTime. time-str)}))

(defn parse-gpx [raw-gpx-str]
  (for [[_ header body] (re-seq trekpoint-regex raw-gpx-str)]
    (parse-trekpoint header body)))

(defn- radians [angle]
  (-> angle (* Math/PI) (/ 180.0)))

;; Euclidean distance would have been fine, since we are going to be using
;; points that are close together.
(defn- haversine-distance
  "Calculates the distance in miles between two points on Earth, using the
  so-called Haversine formula. The input points must have latitude and
  longitudes measured in degrees."
  [point-1 point-2]
  (let [lat-1 (radians (:latitude point-1))
        lat-2 (radians (:latitude point-2))
        lon-1 (radians (:longitude point-1))
        lon-2 (radians (:longitude point-2))
        first-squared-sine (Math/pow (Math/sin (/ (- lat-2
                                                     lat-1)
                                                  2))
                                     2.0)
        second-squared-sine (Math/pow (Math/sin (/ (- lon-2
                                                      lon-1)
                                                   2))
                                      2.0)
        cosines-product (* (Math/cos lat-1)
                           (Math/cos lat-2))
        square-rooted-sum (Math/sqrt (+ first-squared-sine
                                        (* cosines-product
                                           second-squared-sine)))
        earth-radius 3958.8]
    (* 2 earth-radius (Math/asin square-rooted-sum))))

(defn assoc-distances
  "Associates the cumulative distance to each point."
  [points]
  (let [distances (->> points
                       (partition 2 1)
                       (map (fn [[p1 p2]]
                              (haversine-distance p1 p2)))
                       (reductions + 0.0))]
    (vec (map (fn [point distance]
                (assoc point :distance distance))
              points
              distances))))

(defn time-diff [p1 p2]
  (/ (t/in-millis (t/interval (:time p1)
                              (:time p2)))
     1000.0))

(defn pace->str [pace]
  (let [minutes (int (quot pace 60))
        seconds (mod pace 60)]
    (format "%d:%.2f" minutes seconds)))
