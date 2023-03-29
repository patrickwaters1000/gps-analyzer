(ns gps-data.fastest-intervals
  (:require
    [clj-time.core :as t]
    [clojure.pprint :as pprint]
    [clojure.string :as string]
    [gps-data.cli :as cli]
    [gps-data.core :as core])
  (:gen-class))

;; Build:
;;
;; > lein uberjar
;;
;; Run:
;;
;; > ./run.sh fastest-intervals <ARGS>

(defn- can-find-interval-endpoint? [distance points]
  (let [p1 (first points)
        p2 (last points)
        total-distance (- (:distance p2)
                          (:distance p1))]
    (<= distance total-distance)))

(defn- find-interval-endpoint-idx
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
            time (core/time-diff p1 p2)
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
    (if (zero? num-intervals)
      (sort-by :start-time t/before? results)
      (let [interval (fastest-interval interval-distance points-lists)
            new-points-lists (vec (mapcat #(remove-interval-from-points interval %)
                                          points-lists))]
        (recur (conj results interval)
               (dec num-intervals)
               new-points-lists)))))

(defn run [input-file num-intervals interval-distance])

(def cli-spec
  [{:short "-f"
    :long "--input-file"
    :description "Analyze this GPX file."}
   {:short "-n"
    :long "--num-intervals"
    :description "Calculate the fastest 'n' interval times."
    :parse-fn #(Integer/parseInt %)}
   {:short "-d"
    :long "--interval-distance"
    :description "Calculate times for 'd' mile intervals"
    :parse-fn #(Double/parseDouble %)}])

(defn -main [& args]
  (let [{:keys [input-file
                num-intervals
                interval-distance]} (cli/parse-args cli-spec args)
        fastest-intervals
          (->> (slurp input-file)
               core/parse-gpx
               core/assoc-distances
               (fastest-intervals num-intervals
                                  interval-distance)
               (map #(update % :pace core/pace->str))
               (map #(dissoc % :start-time)))]
    (pprint/pprint fastest-intervals)))
