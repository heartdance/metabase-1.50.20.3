(ns metabase.query-processor.init
  (:require
    [metabase.query-processor.middleware.system-parameters :as system-parameters]
    [metabase.query-processor.middleware.array-field-expression-adapt :as array-field-expression-adapt]
    [metabase.query-processor.middleware.time-to-number :as time-to-number]))

(defn init []
  (system-parameters/init)
  (array-field-expression-adapt/init)
  (time-to-number/init)
  )
