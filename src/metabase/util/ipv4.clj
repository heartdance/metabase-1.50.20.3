(ns metabase.util.ipv4)

;; 将IP字符串解析为整数
(defn ip-to-int [ip]
  (let [octets (map #(Integer/parseInt %) (clojure.string/split ip #"\."))]
    (reduce (fn [acc octet]
              (+ (bit-shift-left acc 8) octet))
            0
            octets)))

;; 将整数转换为IP字符串
(defn int-to-ip [ip-int]
  (let [octets (map #(bit-and (bit-shift-right ip-int (* 8 %)) 0xFF)
                    (range 3 -1 -1))]
    (clojure.string/join "." octets)))

;; 掩码长度转换为掩码整数
(defn mask-length-to-int [mask-length]
  (bit-shift-left -1 (- 32 mask-length)))

;; 计算起始IP和终止IP
(defn calc-ip-range [ip mask-length]
  (let [ip-int (ip-to-int ip)
        mask-int (mask-length-to-int mask-length)
        start-ip (bit-and ip-int mask-int)
        end-ip (bit-or start-ip (bit-not mask-int))]
    {:start-ip (int-to-ip start-ip)
     :end-ip (int-to-ip end-ip)}))

;; 示例
(defn -main []
  (let [ip "192.168.1.10"
        mask-length 24]
    (println (calc-ip-range ip mask-length))))
