(ns mishmash.stringy)

(defn hexify 
  "Convert string to hex string"
  [s]
  (apply str
    (map #(format "%02x" (int %)) s)))

(defn unhexify
  "Convert hex string to string"
  [hex]
  (apply str
    (map 
      (fn [[x y]] (char (Integer/parseInt (str x y) 16))) 
        (partition 2 hex))))

