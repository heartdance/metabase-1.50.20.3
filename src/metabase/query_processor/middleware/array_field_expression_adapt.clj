(ns metabase.query-processor.middleware.array-field-expression-adapt
  "Middleware for adapt array field expression in queries."
  (:require
   [clojure.string :as str]
   [metabase.config :as config]
   [metabase.util.log :as log]
   [clojure.java.io :as io]
   [cheshire.core :as json]))

(def array-fields (atom {}))

(defn- load-array-fields-from-dir [dir]
  (if (.isDirectory dir)
    (reduce
      (fn [result fields]
        (merge result fields))
      {}
      (for [file (file-seq dir)
            :when (.endsWith (.getName file) ".json")]
        (->> file
             (.getPath)
             (slurp)
             (json/parse-string)
             (#(% "columns"))
             (filter #(% "array"))
             (reduce (fn [dict field]
                       (assoc dict (str/replace (field "name") \. \_) (field "type")))
                     {}
                     ))
        ))
    {}
    )
  )

(defn load-array-fields [path]
  (let [dir (io/file path)]
    (if (.isDirectory dir)
      (let [oldVersionFields (load-array-fields-from-dir dir)
            basicFields (load-array-fields-from-dir (io/file path "basic"))
            protocolFields (load-array-fields-from-dir (io/file path "protocol"))
            standaloneFields (load-array-fields-from-dir (io/file path "standalone"))]
        (merge oldVersionFields basicFields protocolFields standaloneFields)
        )
      (do
        (log/error "load array fields failed, schema dir not found: " path)
        #{}
        )
      )
    )
  )

(defn init []
  (reset! array-fields (load-array-fields (config/config-str :mb-ch-schema-path)))
  )

(defn- blank [c]
  (or (= c \space) (= c \newline) (= c \tab))
  )

(defn- equals-ignore-case[a b]
  (= (str/lower-case a) (str/lower-case b))
  )

(defn- find-not-before[sql end]
  (loop [i end]
    (if (>= i 2)
      (let [c (nth sql i)]
        (cond
          (blank c)
          (recur (dec i))

          (and (or (= c \t) (= c \T))
               (or (= (nth sql (dec i)) \o) (= (nth sql (dec i)) \O))
               (or (= (nth sql (- i 2)) \n) (= (nth sql (- i 2)) \N))
               (and (> i 2) (blank (nth sql (- i 3)))))
          (- i 2)

          :else
          -1
          )
        )
      -1
      )
    )
  )

(defn- find-word-after[sql start word]
  (loop [i start]
    (if (< (+ i (count word)) (count sql))
      (let [c (nth sql i)]
        (cond
          (blank c)
          (recur (inc i))

          (and (equals-ignore-case (subs sql i (+ i (count word))) word)
               (and (< (+ i (count word)) (count sql)) (blank (nth sql (+ i (count word))))))
          (+ i 2)

          :else
          -1
          )
        )
      -1
      )
    )
  )

(defn- find-operators [sql]
  (loop [i 0 quote nil parts []]
    (if (< i (count sql))
      (let [c (nth sql i)]
        (if (nil? quote)
          (cond
            (= c \=)
            (if (and (> i 0) (= (nth sql (dec i)) \!))
              (recur (inc i) nil (conj parts {:op "!=" :start (dec i) :end (inc i)}))
              (recur (inc i) nil (conj parts {:op "=" :start i :end (inc i)}))
              )

            (or (= c \>) (= c \<))
            (if (and (< (inc i) (count sql)) (= (nth sql (inc i)) \=))
              (recur (+ i 2) nil (conj parts {:op (str c "=") :start i :end (+ i 2)}))
              (recur (inc i) nil (conj parts {:op (str c) :start i :end (inc i)}))
              )

            (or (= c \') (= c \`))
            (recur (inc i) c parts)

            (blank c)
            (cond
              (and (< (+ i 6) (count sql)) (blank (nth sql (+ i 5))) (equals-ignore-case (subs sql (inc i) (+ i 5)) "like"))
              (let [not-idx (find-not-before sql (dec i))]
                (if (= not-idx -1)
                  (recur (+ i 6) nil (conj parts {:op "like" :start i :end (+ i 6)}))
                  (recur (+ i 6) nil (conj parts {:op "not like" :start (dec not-idx) :end (+ i 6)}))
                  )
                )

              (and (< (+ i 4) (count sql)) (blank (nth sql (+ i 3))) (equals-ignore-case (subs sql (inc i) (+ i 3)) "is"))
              (let [not-end (find-word-after sql (+ i 4) "not")]
                (if (= not-end -1)
                  (recur (+ i 4) nil (conj parts {:op "is" :start i :end (+ i 4)}))
                  (recur (+ not-end 1) nil (conj parts {:op "is not" :start i :end (+ not-end 1)}))
                  )
                )

              :else
              (recur (inc i) nil parts)
              )

            :else
            (recur (inc i) nil parts)
            )
          (cond
            (= c quote)
            (recur (inc i) nil parts)

            (= c \\)
            (recur (+ i 2) quote parts)

            :else
            (recur (inc i) quote parts)
            )
          )
        )
      parts
      )
    )
  )

(defn- find-key [sql op]
  (loop [i (dec (:start op)) status 0 end (:start op)]
    (if (>= i 0)
      (let [c (nth sql i)]
        (cond
          (= status 0)
          (cond
            (blank c)
            (recur (dec i) 0 i)

            (= c \`)
            (recur (dec i) 1 i)

            (re-matches #"\w" (str c))
            (recur (dec i) 2 (inc i))

            :else
            nil
            )

          (= status 1)
          (if (= c \`)
            {:key (subs sql (inc i) end) :start i :quote true}
            (recur (dec i) 1 end)
            )

          (= status 2)
          (if (or (re-matches #"\w" (str c)) (= c \.))
            (recur (dec i) 2 end)
            {:key (subs sql (inc i) end) :start (inc i)}
            )

          :else
          nil
          )
        )
      (if (and (= status 2) (not= end 0))
        {:key (subs sql 0 end) :start 0}
        )
      )
    )
  )

(defn- find-value [sql op]
  (loop [i (:end op) status 0 start (:end op)]
    (if (< i (count sql))
      (let [c (nth sql i)]
        (cond
          (= status 0)
          (cond
            (blank c)
            (recur (inc i) 0 i)

            (= c \')
            (recur (inc i) 1 i)

            (or (re-matches #"\w" (str c)) (= c \.) (= c \-))
            (recur (inc i) 2 i)

            :else
            nil
            )

          (= status 1)
          (if (= c \')
            {:value (subs sql start (inc i)) :end (inc i)}
            (if (= c \\)
              (recur (inc i) 11 start)
              (recur (inc i) 1 start)
              )
            )

          (= status 11)
          (recur (inc i) 1 start)

          (= status 2)
          (if (or (re-matches #"\w" (str c)) (= c \.))
            (recur (inc i) 2 start)
            {:value (subs sql start i) :end i}
            )

          :else
          nil
          )
        )
      (if (= status 2)
        {:value (subs sql start i) :end i}
        nil)
      )
    )
  )

(defn- find-expressions [sql]
  (let [parts (find-operators sql)]
    (->> parts
         (map (fn [op]
                (let [exprKey (find-key sql op)
                      exprValue (find-value sql op)]
                  (if (or (nil? exprKey) (nil? exprValue))
                    nil
                    {:start (:start exprKey)
                     :end (:end exprValue)
                     :key (:key exprKey)
                     :quote (:quote exprKey)
                     :value (:value exprValue)
                     :op (:op op)}
                    )
                  )
                ))
         (filter #(and (not (nil? %)) (contains? @array-fields (:key %))))
         (map #(assoc % :type (get @array-fields (:key %))))
         (map #(if (:quote %) (assoc % :key (str "`" (:key %) "`")) %))
         (vec)
         )
    )
  )

(defn- build-expression [{:keys [key op value type]}]
  (cond
    (equals-ignore-case value "null")
    (str (if (or (= op "!=") (= op "is not")) "notEmpty" "empty") "(" key ")")

    (= op "=")
    (str "has(" key "," (if (= type "IPv4") (str "toIPv4(" value ")") value) ")")

    (= op "!=")
    (str "not has(" key "," (if (= type "IPv4") (str "toIPv4(" value ")") value) ")")

    :else
    (str "arrayExists(x -> x " op " " (if (= type "IPv4") (str "toIPv4(" value ")") value) "," key ")")
    )
  )

(defn- adapt-field-array-expressions [sql]
  (let [parts (find-expressions sql)]
    (if (empty? parts)
      sql
      (loop [i 0 last-end 0 result ""]
        (if (< i (count parts))
          (let [part (parts i)
                last-part-sql (subs sql last-end (:start part))
                expression (build-expression part)]
            (recur (inc i) (:end part) (str result last-part-sql expression))
            )
          (str result (subs sql last-end))
          )
        )
      )
    )
  )

(defn adapt-expression
  "Adapt Dashboard or Card-supplied array field expression in a query.

  A SQL query with a array field expression like `protocols='http'` will have that part of the query replaced with
  `has(protocols, 'http')`."
  [query]
  (let [sql (-> query :native :query)
        new-sql (adapt-field-array-expressions sql)]
    (if (= sql new-sql)
      query
      (do
        (log/debug "adapt expression sql '" sql "' to '" new-sql "'")
        (assoc-in query [:native :query] new-sql)
        )
      )
    ))
