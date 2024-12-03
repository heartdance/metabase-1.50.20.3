(ns metabase.query-processor.init
  (:require
    [metabase.query-processor.middleware.system-parameters :as system-parameters]
    [metabase.query-processor.middleware.array-field-expression-adapt :as array-field-expression-adapt]))

(defn init []
  (system-parameters/init)
  (array-field-expression-adapt/init)
  )
