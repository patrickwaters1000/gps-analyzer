(ns gps-data.api
  (:require
    [gps-data.fastest-intervals :as fastest-intervals])
  (:gen-class))

(defn -main [& args]
  (case (first args)
    "fastest-intervals" (apply fastest-intervals/-main (rest args))))
