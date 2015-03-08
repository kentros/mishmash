(ns mishmash.truthy)

(defn xor?
  "Exclusive or.  Returns true iff an odd number of args are logical true, otherwise returns false."
  [& args]
  (odd? (count (filter identity args))))
