(ns gps-data.cli)

(defn- get-arg-spec [cli-spec arg]
  (->> cli-spec
       (filter #(or (= (:short %) arg)
                    (= (:long %) arg)))
       first))

(defn parse-args [cli-spec args]
  (->> args
       (partition 2)
       (map (fn [[k v]]
              (let [{:keys [long parse-fn]} (get-arg-spec cli-spec k)
                    [_ k*] (re-matches #"\-\-(.+)" long)
                    v* (if parse-fn
                         (parse-fn v)
                         v)]
                [(keyword k*) v*])))
       (into {})))
