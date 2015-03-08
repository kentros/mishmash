(ns mishmash.stringy
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math])
  (:use [incanter.stats :only [levenshtein-distance]]))

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

(defn strip
  "Returns string that represents the first string argument stripped of any characters in the second argument"
  [coll chars]
  (apply str (remove #((set chars) %) coll)))

(defn devowel
  "Removes vowels a, e, i, o, and u (both cases) from the given string."
  [s]
  (s/join (re-seq #"[^aeiouAEIOU]" s)))

(defn avg-length
  "Returns the average length of the strings in the given collection."
  [coll]
  (if-not (empty? coll) (math/round (/ (reduce + (map #(.length %1) coll)) (count coll))) 0))

(defn numeric?
  "Returns true if the string is a numeric string, including floating point and negative numbers."
  [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(defn english-number
  "Returns English representation of a number if a is a valid number.  If the given string is not numeric, the
  string itself is returned back."
  [a]
  (if (numeric? a) (clojure.pprint/cl-format nil "~R" (Long/valueOf a)) a))

(defn roman-numeral
  "Returns Roman numeral representation of a number if valid.  Using the accepted convention of never
  repeating a symbol more than three times in succession, 3999 (MMMCMXCIX) may be as high as this can go.  Going
  above 3999 requires using conventions (bars/special characters) that can not be easily represented.
  If the given string is not numeric, the string itself is returned back. "
  [a]
  (if (numeric? a) (clojure.pprint/cl-format nil "~@R" (Long/valueOf a)) a))

(defn almost-equals?
  "Returns true if the two given strings almost match.  This is a subjective guess and several approaches may be used.
  The optional third argument can be used to control how loose acceptance can be.  It's more permissive as it
  approaches 0.  A value of 1.0 represents the least permissive configuration."
  [a b & {:keys [conf] :or {conf 0.65}}]
  (let [str1 (s/lower-case a)
        str2 (s/lower-case b)]
    (or
      (>=  (- 1 (/ (levenshtein-distance str1 str2) (avg-length [str1 str2]))) conf)
      (.equalsIgnoreCase str1 str2))))

(defn extract-ips
  "Returns a lazy sequence of successive matches of IP addresses in string."
  [s]
  (re-seq #"\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b" s))