(ns metabase.driver.sql.query-processor.auto-use-array-join
  (:require
    [clojure.string :as str]))

(defn- is-array-col [col]
  (if (and (vector? col)
           (= (count col) 3)
           (= (first col) :metabase.util.honey-sql-2/typed)
           (vector? (nth col 1))
           (map? (nth col 2)))
    (let [col-field (nth col 1)
          col-option (nth col 2)
          db-type (:database-type col-option)]
      (and (= (count col-field) 3)
           (= (first col-field) :metabase.util.honey-sql-2/identifier)
           (= (nth col-field 1) :field)
           (vector? (nth col-field 2))
           (string? db-type)
           (str/starts-with? db-type "array(")
           (str/ends-with? db-type ")"))
      )
    false
    )
  )

(defn- extract-col-name [col]
  (let [fields (-> col (nth 1) (nth 2))]
    (->> fields
         (map #(str "`" % "`"))
         (str/join "."))
    )
  )

(defn- array-col-to-basic [col]
  (let [fields (-> col (nth 1) (nth 2))
        db-type (-> col (nth 2) (:database-type))]
    [:metabase.util.honey-sql-2/typed
     [:metabase.util.honey-sql-2/identifier :field [(last fields)]]
     {:database-type (subs db-type 6 (dec (count db-type)))}
     ]
    )
  )

(defn- replace-select-item [item]
  (if (> (count item) 0)
    (let [col (nth item 0)]
      (if (is-array-col col)
        (assoc item 0 [:raw (str "arrayJoin(" (extract-col-name col) ")")])
        item
        )
      )
    item
    )
  )

(defn- replace-select [select]
  (if (or (nil? select) (empty? select))
    select
    (->> select
         (map replace-select-item)
         (vec))
    )
  )

(defn- replace-group-by-item [item]
  (if (is-array-col item)
    (array-col-to-basic item)
    item
    )
  )

(defn- replace-group-by [group-by]
  (if (or (nil? group-by) (empty? group-by))
    group-by
    (->> group-by
         (map replace-group-by-item)
         (vec))
    )
  )

(defn- replace-order-by-item [item]
  (if (> (count item) 0)
    (let [col (nth item 0)]
      (if (is-array-col col)
        (assoc item 0 (array-col-to-basic col))
        item
        )
      )
    item
    )
  )

(defn- replace-order-by [order-by]
  (if (or (nil? order-by) (empty? order-by))
    order-by
    (->> order-by
         (map replace-order-by-item)
         (vec))
    )
  )

(defn auto-use-array-join [struct]
  (if (map? struct)
    (let [{:keys [select group-by order-by]} struct]
      (if (or (nil? group-by) (empty? group-by))
        struct
        (assoc struct :select (replace-select select)
                      :group-by (replace-group-by group-by)
                      :order-by (replace-order-by order-by))
        )
      )
    struct
    )
  )
