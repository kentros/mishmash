(ns mishmash.stringy-test
  (:require [clojure.test :refer :all]
            [mishmash.stringy :refer :all]))

(deftest hexify-test
  (testing "Tests strings can be converted to hex"
    (is (= "foo" (unhexify(hexify "foo"))))))
