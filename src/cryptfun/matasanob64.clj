(ns cryptfun.matasanob64)

;; test input: hex-encoded byte sequence
(def istr "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

;; create a hashmap from all 64 6 bit sequences to ASCII characters
(def tqbytes (into [] (for [i [0 1] j [0 1] k [0 1] l [0 1] m [0 1] n [0 1]] [i j k l m n])))
(def b64table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(def b64map (apply hash-map (interleave tqbytes b64table)))
;; Example mapping: (first b64map) ==> [[1 0 1 1 0 1] \t]

;; create a hashmap from all 16 ascii-encoded hex characters to their binary bit sequences 
(def hx (concat (map char (range 48 58)) (map char (range 97 103))))
(def halfbytes (into [] (for [i [0 1] j [0 1] k [0 1] l [0 1]] [i j k l])))
(def halfbytemap (apply hash-map (interleave hx halfbytes)))
;; Example mapping: (first halfbytemap) ==> [\a [1 0 1 0]]

;; initial naive implementation without padding implemented
(defn b64-nopad [s]
  (let [bitseq (flatten (map halfbytemap s)) ;; convert to bit sequence
        p6 (partition-all 6 bitseq)]         ;; break into 6 bit chunks
    (apply str (map b64map p6))))            ;; lookup each chunk


;;; padding utilities

;; "Add extra bytes with value zero so there are three bytes, and
;; perform the conversion to base64." -- Wikipedia
(defn zeropadinput [s]
  (->> s
       (partition 6 6 "000000")
       flatten
       (apply str)))

;; pad with either "=" or "==" if input is not a multiple of 3 bytes.
(defn equalpad [origstr outstr]
  (if (= 2 (mod (count origstr) 6))
    (apply str (concat (take 2 outstr) "=="))
    (if (= 4 (mod (count origstr) 6))
      (apply str (concat (take 3 outstr) "="))
      outstr)))

;; use equalpad to update final result
(defn applyequalpad [s r]
  (let [epl4 (equalpadlast4 s (take-last 4 r))]
    (apply str (concat (drop-last 4 r) epl4))))

;; full working B64 encoder with padding
(defn b64
  "Encode string s using BASE64"
  [s]
  (->> s
       zeropadinput
       (map halfbytemap)
       flatten
       (partition-all 6)
       (map b64map)
       (apply str)
       (applyequalpad s)))

;; test matasano example
(= (b64 istr) "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

;; test wikipedia examples

;; utils to conver ascii-hex
(defn bytes->hex
  "convert from an array of ints to a hex encoded string"
  [b]
  (let [newmsghex (map #(Integer/toHexString %) b)]
    (apply str (map #(if (= 1 (count %)) (str "0" %) %) newmsghex))))

(defn ascii->hex
  "convert an ascii string to hex encoding"
  [s]
  (->> (seq s)
       (map int)
       bytes->hex))

;; wikipedia examples
(= (b64 (ascii->hex "any carnal pleasure."))
    "YW55IGNhcm5hbCBwbGVhc3VyZS4=")
(= (b64 (ascii->hex "any carnal pleasure"))
    "YW55IGNhcm5hbCBwbGVhc3VyZQ==")
(= (b64 (ascii->hex "any carnal pleasur"))
    "YW55IGNhcm5hbCBwbGVhc3Vy")
(= (b64 (ascii->hex "any carnal pleasu"))
    "YW55IGNhcm5hbCBwbGVhc3U=")
(= (b64 (ascii->hex "any carnal pleas"))
    "YW55IGNhcm5hbCBwbGVhcw==")
(= (b64 (ascii->hex "pleasure."))
    "cGxlYXN1cmUu")
(= (b64 (ascii->hex "leasure."))
    "bGVhc3VyZS4=")
(= (b64 (ascii->hex "easure."))
    "ZWFzdXJlLg==")
(= (b64 (ascii->hex "asure."))
    "YXN1cmUu")
(= (b64 (ascii->hex "sure."))
    "c3VyZS4=")

(= (b64 (ascii->hex "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."))
   "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
