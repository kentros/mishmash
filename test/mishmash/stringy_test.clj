(ns mishmash.stringy-test
  (:require [clojure.test :refer :all]
            [mishmash.stringy :refer :all]))

(deftest hexify-test
  (testing "Tests strings can be converted to hex"
    (is (= "foo" (unhexify(hexify "foo"))))))

(deftest almost-equals?-test
  (testing "Tests some strings that should register almost equal and some that should not"
    (is (almost-equals? "the" "THE"))
    (is (almost-equals? "their own" "there own"))
    (is (almost-equals? "it's" "its"))
    (is (almost-equals? "vacuum" "vacume"))
    (is (almost-equals? "dwarfs" "dwarves"))
    (not (almost-equals? "pie" "pi"))
    (not (almost-equals? "Missouri River" "Mississppi River"))))

(deftest numeric?-test
  (testing "Tests some strings that should register numeric and some that should not"
    (is (numeric? "1234567890"))
    (is (numeric? "-1.0"))
    (not (numeric? "schfify-five"))))

(deftest devowel-test
  (testing "Tests appropriate averages of the strings in the given sequences are correct"
    (is (= "ld McDnld hd  frm, ." (devowel "Old McDonald had a farm, EIEIO.")))))

(deftest avg-length-test
  (testing "Tests appropriate averages of the strings in the given sequences are correct"
    (is (= 0 (avg-length nil)))
    (is (= 0 (avg-length '())))
    (is (= 1 (avg-length '("1"))))
    (is (= 5 (avg-length '["0123456789" "12" "123"])))))

(deftest english-number-test
  (testing "Tests numeric strings convert to appropriate english numbers, spelled out"
    (is (= "twelve" (english-number "12")))
    (is (= "five hundred twenty-five thousand, six hundred" (english-number "525600")))))

(deftest roman-numeral-test
  (testing "Tests strings convert to appropriate roman numerals"
    (is (= "IV" (roman-numeral "4")))
    (is (= "IX" (roman-numeral "9")))))

(deftest extract-ips-test
  (testing "Tests strings have IPs extracted"
    (is (= "111.123.0.127" (first(extract-ips "Here's an ip:111.123.0.127, and another 136.54.23.108.."))))))
