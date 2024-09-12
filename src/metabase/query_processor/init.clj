(ns metabase.query-processor.init
  (:require
    [metabase.query-processor.middleware.system-parameters :as system-parameters]))

(defn init []
  (system-parameters/init)
  )
