(ns metabase.server.middleware.offset-paging
  (:require
   [medley.core :as m]))

(set! *warn-on-reflection* true)

(def ^:dynamic *limit* "Limit for offset-limit paging." nil)
(def ^:private default-limit 50)

(def ^:dynamic *offset* "Offset for offset-limit paging." nil)
(def ^:private default-offset 0)

(def ^:dynamic *paged?*
  "Bool for whether a request is paged or not.
  Automatically generated by a handler in offset-paging middleware."
  false)

(defn- parse-paging-params [{{:strs [limit offset]} :query-params}]
  (let [limit  (some-> limit parse-long)
        offset (some-> offset parse-long)]
    (when (or limit offset)
      {:limit (or limit default-limit), :offset (or offset default-offset)})))

(defn- with-paging-params [request {:keys [limit offset]}]
  (-> request
      (assoc ::limit limit, ::offset offset)
      (m/dissoc-in [:query-params "offset"])
      (m/dissoc-in [:query-params "limit"])
      (m/dissoc-in [:params :offset])
      (m/dissoc-in [:params :limit])))

(defn handle-paging
  "Limit offset paging.
  This has many downsides but many upsides, chief among them at-will random paging.
  (it isn't stable with respect to underlying data changing, though)"
  [handler]
  (fn [request respond raise]
    (if-let [{:keys [limit offset] :as paging-params} (parse-paging-params request)]
      (binding [*limit* limit
                *offset* offset
                *paged?* true]
        (handler (with-paging-params request paging-params) respond raise))
      (handler request respond raise))))