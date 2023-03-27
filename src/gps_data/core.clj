(ns gps-data.core
  (:require
    [clj-time.core :as t]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [gps-data.plot :as plot])
  (:import
    (java.util.regex Pattern)
    (org.joda.time DateTime)))

(def trekpoint-regex (Pattern/compile "<trkpt(.+?)>(.+?)</trkpt>" Pattern/DOTALL))
(def latitude-regex (Pattern/compile "lat=\"(.+?)\"" Pattern/DOTALL))
(def longitude-regex (Pattern/compile "lon=\"(.+?)\"" Pattern/DOTALL))
(def elevation-regex (Pattern/compile "<ele>(.+?)</ele>" Pattern/DOTALL))
(def time-regex (Pattern/compile "<time>(.+?)</time>" Pattern/DOTALL))

(defn parse-trekpoint [header body]
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
(defn haversine-distance
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

(defn can-find-interval-endpoint? [distance points]
  (let [p1 (first points)
        p2 (last points)
        total-distance (- (:distance p2)
                          (:distance p1))]
    (<= distance total-distance)))

(defn find-interval-endpoint-idx
  "Returns the first index defining a distance of at least `interval` from the
  start point, assuming such an endpoint exists. The calling code is responsible
  for checking that this is the case."
  [interval-distance endpoint-idx-guess points]
  {:pre [(vector? points)
         (pos? endpoint-idx-guess)]}
  (let [p1 (first points)
        p2 (nth points (dec endpoint-idx-guess))
        p3 (nth points endpoint-idx-guess)
        distance-12 (- (:distance p2) (:distance p1))
        distance-13 (- (:distance p3) (:distance p1))]
    (cond
      (>= distance-12 interval-distance)
        (find-interval-endpoint-idx interval-distance
                                    (dec endpoint-idx-guess)
                                    points)
      (< distance-13 interval-distance)
        (find-interval-endpoint-idx interval-distance
                                    (inc endpoint-idx-guess)
                                    points)
      :else endpoint-idx-guess)))

(defn time-diff [p1 p2]
  (/ (t/in-millis (t/interval (:time p1)
                              (:time p2)))
     1000.0))

(defn find-intervals [interval-distance points]
  {:pre [(vector? points)
         (pos? (count points))]}
  (loop [results []
         points points
         endpoint-idx-guess 1]
    (if-not (try
              (can-find-interval-endpoint? interval-distance points)
              (catch Exception e
                (throw (Exception. (str {:interval-distance interval-distance
                                         :points points})))))
      results
      (let [endpoint-idx (find-interval-endpoint-idx interval-distance
                                                     endpoint-idx-guess
                                                     points)
            p1 (first points)
            p2 (nth points endpoint-idx)
            distance (- (:distance p2) (:distance p1))
            time (time-diff p1 p2)
            pace (/ time distance)
            new-result {:start (:distance p1)
                        :start-time (:time p1)
                        :distance distance
                        :interval-time time
                        :pace pace}
            new-endpoint-idx-guess (->> endpoint-idx
                                        (max 1)
                                        (min (- (count points) 2)))]
        (recur (conj results new-result)
               (vec (rest points))
               new-endpoint-idx-guess)))))

(defn fastest-interval [interval-distance points-lists]
  (->> points-lists
       (mapcat (fn [points]
                 (println (class points))
                 (find-intervals interval-distance points)))
       (sort-by :pace)
       first))

(defn remove-interval-from-points
  "If the interval is not in `points` returns points. Otherwise, removes the
  interval from points and returns the remaining connected intervals. Usually
  there would be two, but it's possible there could be one or none."
  [interval points]
  (let [start-time (:start-time interval)
        end-time (t/plus start-time (t/millis (* 1000 (:interval-time interval))))
        points-1 (filterv #(t/before? (:time %) start-time) points)
        points-2 (filterv #(not (t/before? (:time %) end-time)) points)]
    (->> [points-1 points-2]
         (remove #(< (count %) 2))
         vec)))

(defn fastest-intervals [num-intervals interval-distance points]
  (loop [results []
         num-intervals num-intervals
         points-lists [points]]
    (print (map count points-lists))
    (if (zero? num-intervals)
      (sort-by :start-time t/before? results)
      (let [interval (fastest-interval interval-distance points-lists)
            new-points-lists (vec (mapcat #(remove-interval-from-points interval %)
                                          points-lists))]
        (recur (conj results interval)
               (dec num-intervals)
               new-points-lists)))))

(defn pace->str [pace]
  (let [minutes (int (quot pace 60))
        seconds (mod pace 60)]
    (format "%d:%.2f" minutes seconds)))

(defn calculate-fastest-intervals [input-file num-intervals interval-distance]
  (->> (slurp input-file)
       parse-gpx
       assoc-distances
       (fastest-intervals num-intervals interval-distance)
       (map #(update % :pace pace->str))
       (map #(dissoc % :start-time))))

(comment
  (calculate-fastest-intervals (io/resource "Crushing_my_mile_PR.gpx")
                               1
                               1.0)
  (calculate-fastest-intervals (io/resource "Mile_intervals.gpx")
                               3
                               1.0)
  ;;
  )
