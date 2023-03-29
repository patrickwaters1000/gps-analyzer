(ns gps-data.plot
  (:require [cheshire.core :as json]
            [clj-time.core :as t]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn normalize-template-key [s]
  (format "{{%s}}"
          (-> (name s)
              string/upper-case
              (string/replace "-" "_"))))

(defn render-string
  [template params]
  (reduce-kv (fn [acc param-name param-val]
               (string/replace acc
                               (normalize-template-key param-name)
                               (if (keyword? param-val)
                                 (name param-val)
                                 (str param-val))))
             template
             params))

;; TODO Improve function name.
;; NOTE(patrick) Generally I use `generate-plots!`
(defn generate! [opts-maps output-file]
  (let [template (slurp (io/resource "plot_template.html"))
        js (->> opts-maps
                (map #(format "addPlot(%s);"
                              (json/generate-string %)))
                (string/join "\n"))
        page (render-string template {:add-plot-calls js})
        dir (System/getenv "HOME")
        path (format "%s/plots/%s" dir output-file)]
    (spit path page)
    path))
