
(ns danilomo.cal
  (:require [table.core :as t]
            [clojure.string :as s]
            [clojure.tools.cli :refer [parse-opts]])
  (:import [java.time LocalDate]
           [java.time.temporal TemporalAdjusters]))

(defn year-header [year]
  (str "                                " year))

(defn date [day month year]
  (LocalDate/of year month day))

(def MONTHS ["      January"
             "      February"
             "       March"
             "       April"
             "        May"
             "       June"
             "       July"
             "      August"
             "      September"
             "      October"
             "      November"
             "      December"])
(def MONTH-HEADER "Su Mo Tu We Th Fr Sa")

(def inc7 (partial + 7))

(defn parse-row [start last-day-of-month]
  (map #(cond (or (>= 0 %) (< last-day-of-month %)) "  "
              (<= % 9) (str " " %)
              :else (str %)) (range start (inc7 start))))

(def table-options  {:skip-header true :style (t/with-separator "     ")})

(defn cal-for-month [month year]
  (let [dt (date 1 month year)
        day-of-week-first-day (dec (-> dt .getDayOfWeek .getValue))
        last-day-of-month (.getDayOfMonth (.with dt (TemporalAdjusters/lastDayOfMonth)))
        range (take-while
               #(>= last-day-of-month %)
               (iterate inc7 (- day-of-week-first-day)))]
    (map #(parse-row % last-day-of-month) range)))

(defn join-lines [lines]
  (remove s/blank? (map #(s/join " " %) lines)))

(defn zip-longest [& sequences]
  (let [longest-size (apply max (map count sequences))
        transformed-seqs (map #(concat % (repeat "")) sequences)]
    (take longest-size (apply map vector transformed-seqs))))

(defn cal-row [months year]
  (let [entries (apply zip-longest
                       (map #(concat [(get MONTHS %) MONTH-HEADER] (join-lines (cal-for-month (inc %) year)))
                            months))]
    entries))

(defn cal-year [year]
  (println (year-header year))
  (doseq [months (partition 3 (range 0 12))]
    (t/table (cal-row months year) table-options)
    (println)))

(defn cal-month [month year]
  (println (str (subs (get MONTHS (dec month)) 2) " " year))
  (println MONTH-HEADER)
  (println (s/join "\n" (join-lines (cal-for-month month year)))))

(def NOW (LocalDate/now))

(def current-year (.getYear NOW))

(def current-month (-> NOW .getMonth .getValue))

(def cli-options
  [["-y" "--year"]
   ["-m" "--month" :default true]])

(defn -main
  [& args]
  (let [parsed-args (parse-opts args cli-options)
        {month-flag :month year-flag :year} (:options parsed-args)
        [year] (:arguments parsed-args)]
    (cond
      year-flag (cal-year current-year)
      month-flag (cal-month current-month current-year)
      :else (cal-year (Integer/parseInt year)))))

