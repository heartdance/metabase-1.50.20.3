(ns metabase.query-processor.middleware.time-to-number
  "Middleware for time to number in queries."
  (:import [java.time LocalDateTime ZoneOffset]
           [java.time.format DateTimeFormatter])
  (:require
    [clojure.string :as str]))

(def time-fields (atom {}))
(def start-day-of-week 7)

(defn init []
  (reset! time-fields {"ct" "s" "ctms" "ms"})
  )

(defn- with-start-seconds [date-time]
  (-> date-time
      (.withSecond 0)
      (.withNano 0))
  )

(defn- with-start-minutes [date-time]
  (-> date-time
      (with-start-seconds)
      (.withMinute 0))
  )

(defn- with-start-hours [date-time]
  (-> date-time
      (with-start-minutes)
      (.withHour 0))
  )

(defn- with-start-day-of-week [date-time]
  (-> date-time
      (with-start-hours)
      (.minusDays (-> date-time
                      (.getDayOfWeek)
                      (.getValue)
                      (#(mod (+ (- % start-day-of-week) 7) 7))
                      )))
  )

(defn- with-start-day-of-month [date-time]
  (-> date-time
      (with-start-hours)
      (.withDayOfMonth 1))
  )

(defn- with-start-day-of-quarter [date-time]
  (-> date-time
      (with-start-day-of-month)
      (.minusMonths (-> date-time
                        (.getMonthValue)
                        (#(mod (- % 1) 3))
                        )))
  )

(defn- with-start-day-of-year [date-time]
  (-> date-time
      (with-start-hours)
      (.withDayOfYear 1))
  )

(defn- to-start-date-time [date-time unit offset]
  (case unit
    :second (.withNano (.plusSeconds date-time offset) 0)
    :minute (with-start-seconds (.plusMinutes date-time offset))
    :hour (with-start-minutes (.plusHours date-time offset))
    :day (with-start-hours (.plusDays date-time offset))
    :week (with-start-day-of-week (.plusWeeks date-time offset))
    :month (with-start-day-of-month (.plusMonths date-time offset))
    :quarter (with-start-day-of-quarter (.plusMonths date-time (* 3 offset)))
    :year (with-start-day-of-year (.plusYears date-time offset))
    (throw (IllegalArgumentException. (str "unknown unit " unit))))
  )

(defn- to-date-time [date-time unit offset]
  (case unit
    :second (.plusSeconds date-time offset)
    :minute (.plusMinutes date-time offset)
    :hour (.plusHours date-time offset)
    :day (.plusDays date-time offset)
    :week (.plusWeeks date-time offset)
    :month (.plusMonths date-time offset)
    :quarter (.plusMonths date-time (* 3 offset))
    :year (.plusYears date-time offset)
    (throw (IllegalArgumentException. (str "unknown unit " unit))))
  )

(defn- to-range-time [date-time unit offset include-current]
  (if (> offset 0)
    (let [start-time (to-start-date-time date-time unit (if include-current 0 1))
          end-time (to-start-date-time date-time unit (inc offset))]
      {:start start-time :end end-time}
      )
    (let [start-time (to-start-date-time date-time unit offset)
          end-time (to-start-date-time date-time unit (if include-current 1 0))]
      {:start start-time :end end-time}
      )
    )
  )

(def datetime-formatter DateTimeFormatter/ISO_LOCAL_DATE_TIME)

(defn parse-date-time [date-str]
  (if (= (count date-str) 10)
    (LocalDateTime/parse (str date-str "T00:00:00") datetime-formatter)
    (LocalDateTime/parse date-str datetime-formatter)
    )
  )

(defn- temporal-unit [filter]
  (if (and (vector? filter) (> (count filter) 2) (vector? (nth filter 1)))
    (let [field-info (nth filter 1)]
      (if (and (vector? field-info) (> (count field-info) 2) (map? (nth field-info 2)))
        (:temporal-unit (nth field-info 2))
        nil
        )
      )
    nil
    )
  )

(defn- adapt-field-unit [date-time field-time-unit]
  (let [zone-offset (ZoneOffset/ofHours 8)
        epoch-second (.toEpochSecond date-time zone-offset)]
    (cond
      (= field-time-unit "ms")
      (* epoch-second 1000)

      (= field-time-unit "ns")
      (* epoch-second 1,000,000,000)

      :else
      epoch-second
      )
    )
  )

(defn- time-interval-to-number [filter field-unit]
  (let [offset (nth filter 2)
        unit (nth filter 3)
        include-current (or (= offset :current) (and (> (count filter) 4) (map? (nth filter 4)) (true? (:include-current (nth filter 4)))))
        {:keys [start end]} (to-range-time (LocalDateTime/now) unit (if (= offset :current) 0 offset) include-current)
        start-num (adapt-field-unit start field-unit)
        end-num (dec (adapt-field-unit end field-unit))]
    [:between (nth filter 1) start-num end-num]
    )
  )

(defn- relative-time-interval-to-number [filter field-unit]
  (let [offset (nth filter 2)
        unit (nth filter 3)
        from-offset (nth filter 4)
        from-unit (nth filter 5)
        from-date-time (to-date-time (LocalDateTime/now) from-unit from-offset)
        {:keys [start end]} (to-range-time from-date-time unit offset false)
        start-num (adapt-field-unit start field-unit)
        end-num (dec (adapt-field-unit end field-unit))]
    [:between (nth filter 1) start-num end-num]
    )
  )

(defn- between-to-number [filter field-unit]
  (let [start-str (nth filter 2)
        end-str (nth filter 3)
        temporal-unit (temporal-unit filter)
        time-unit (if (nil? temporal-unit) (if (= (count start-str) 10) :day :second) temporal-unit)
        start-time (parse-date-time start-str)
        end-time (parse-date-time end-str)
        start (to-start-date-time start-time time-unit 0)
        end (to-start-date-time end-time time-unit 1)
        start-num (adapt-field-unit start field-unit)
        end-num (dec (adapt-field-unit end field-unit))
        ]
    [:between (nth filter 1) start-num end-num]
    )
  )

(defn- equal-to-number [filter field-unit]
  (let [date-str (nth filter 2)
        date-time (parse-date-time date-str)
        temporal-unit (temporal-unit filter)
        time-unit (if (nil? temporal-unit) (if (= 10 (count date-str)) :day :second) temporal-unit)
        start (to-start-date-time date-time time-unit 0)
        end (to-start-date-time date-time time-unit 1)
        start-num (adapt-field-unit start field-unit)
        end-num (dec (adapt-field-unit end field-unit))
        ]
    [:between (nth filter 1) start-num end-num]
    )
  )

(defn- gt-to-number [filter field-unit]
  (let [date-str (nth filter 2)
        date-time (parse-date-time date-str)
        temporal-unit (temporal-unit filter)
        time-unit (if (nil? temporal-unit) (if (= 10 (count date-str)) :day :second) temporal-unit)
        start (to-start-date-time date-time time-unit 1)
        ]
    [:>= (nth filter 1) (adapt-field-unit start field-unit)]
    )
  )

(defn- lt-to-number [filter field-unit]
  (let [date-str (nth filter 2)
        date-time (parse-date-time date-str)
        temporal-unit (temporal-unit filter)
        time-unit (if (nil? temporal-unit) (if (= 10 (count date-str)) :day :second) temporal-unit)
        start (to-start-date-time date-time time-unit 0)
        ]
    [:< (nth filter 1) (adapt-field-unit start field-unit)]
    )
  )

(defn- transform-time-to-number [filter unit]
  (let [op (first filter)]
    (cond
      (= op :time-interval)
      (time-interval-to-number filter unit)

      (= op :relative-time-interval)
      (relative-time-interval-to-number filter unit)

      (= op :between)
      (between-to-number filter unit)

      (or (= op :=))
      (equal-to-number filter unit)

      (= op :>)
      (gt-to-number filter unit)

      (= op :<)
      (lt-to-number filter unit)

      :else
      filter
      )
    )
  )

(defn- field-time-unit [filter]
  (if (and (vector? filter) (> (count filter) 2) (vector? (nth filter 1)))
    (let [field-info (nth filter 1)]
      (if (and (vector? field-info) (> (count field-info) 2) (map? (nth field-info 2)))
        (:time-unit (nth field-info 2))
        nil
        )
      )
    nil
    )
  )

(defn- time-to-number-in-filter [filter]
  (if (or (nil? filter) (= :and filter) (= :or filter) (empty? filter))
    filter
    (if (or (= :and (first filter)) (= :or (first filter)))
      (->> filter
           (map #(time-to-number-in-filter %))
           (vec)
           )
      (let [unit (field-time-unit filter)]
        (if (nil? unit)
          filter
          (transform-time-to-number filter unit)
          )
        )
      )
    )
  )

(defn- hack-condition-filter [filter]
  (if (and (vector? filter) (> (count filter) 2) (vector? (nth filter 1)))
    (let [field-info (nth filter 1)]
      (if (and (vector? field-info) (> (count field-info) 2) (map? (nth field-info 2)) (= (nth field-info 1) 105))
        (let [field-option (nth field-info 2)
              field-option (assoc field-option :base-type "type/Integer" :time-unit "s")
              field-info (assoc field-info 1 101)
              field-info (assoc field-info 2 field-option)]
          (assoc filter 1 field-info)
          )
        filter
        )
      )
    filter
    )
  )

(defn- hack-filter [filter]
  (if (or (nil? filter) (= :and filter) (= :or filter) (empty? filter))
    filter
    (if (or (= :and (first filter)) (= :or (first filter)))
      (->> filter
           (map #(hack-filter %))
           (vec)
           )
      (hack-condition-filter filter)
      )
    )
  )

(defn time-to-number
  "change time to number."
  [query]
  (let [filter (-> query :query :filter)
        filter-hack (hack-filter filter)
        new-filter (time-to-number-in-filter filter-hack)]
    (do
      (assoc-in query [:query :filter] new-filter)
      )
    ))
