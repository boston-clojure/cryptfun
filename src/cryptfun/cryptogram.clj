(ns cryptfun.cryptogram
  (:use (incanter core stats charts))
  (:require [clojure.string :refer [lower-case split]]))

;; Cryptogram Fun

;; create a random mapping between letters
;; 65-91 is just upper-case letters
;; (def lettermapping
;;   (let [ranges [[65 91]]  ;; [[97 123] [65 91]]
;;         alphas (map char (flatten (map #(apply range %) ranges)))]
;;   (zipmap alphas (shuffle alphas))))

;; basic letter map - one range only
(def lettermap
  (let [alphas (map char (range 65 91))
        mapu (zipmap alphas (shuffle alphas))
        mylower #(char (+ (int %) 0x20))
        mapl (zipmap (map mylower (keys mapu)) (map mylower (vals mapu)))]
    (merge mapu mapl))))

;; create a reverse mapping
(def unmap (zipmap (vals lettermap) (keys lettermap)))

(defn crypt
  "Transform a string s by applying a letter-to-letter mapping. When
  the source letter doesn't appear in the map, then leave it unchanged
  in the result."
  [mapping s]
  (apply str (map #(mapping % %) s)))

;; sample text
(def bunchotext
  "I think that it's extraordinarily important that we in computer science keep fun in computing. When it started out, it was an awful lot of fun. Of course, the paying customers got shafted every now and then, and after a while we began to take their complaints seriously. We began to feel as if we really were responsible for the successful, error-free perfect use of these machines. I don't think we are. I think we're responsible for stretching them, setting them off in new directions, and keeping fun in the house. I hope the field of computer science never loses its sense of fun. Above all, I hope we don't become missionaries. Don't feel as if you're Bible salesmen. The world has too many of those already. What you know about computing other people will learn. Don't feel as if the key to successful computing is only in your hands. What's in your hands, I think and hope, is intelligence: the ability to see the machine as more than when you were first led up to it, that you can make it more.")

(defn pct-frequencies
  "Transform a frequency map from raw occurences to a sorted list of relative percentages."
  [text]
  (let [pct #(double (/ (* %1 100) %2))
        histo (clojure.core/reverse (sort-by val (frequencies text)))]
    (map #(assoc % 1 (pct (% 1) (count text))) histo)))
        
(defn barchart
  "Display an Incanter bar-chart for a frequencies map."
  [fq]
  (view (bar-chart
         (map first fq)
         (map second fq)
         :x-label "Letter"
         :y-label "Relative Frequency")))

(defn ci-lettersonly
  "Transform a string into lowercase letters and remove all non-alphas"
  [s]
  (->> (clojure.string/lower-case s)
       (filter #(<= 97 (int %) 123)))))

(def bunchotextcryp (crypt lettermap bunchotext))

;; chart the relative frequencies in the cryptogram
(barchart (pct-frequencies (ci-lettersonly bunchotextcryp)))
(barchart (pct-frequencies (ci-lettersonly bunchotext)))

;; how about first letters?
(defn firstletters
  [s]
  (->> (split s #" ")
       (map first)
       (map lower-case)
       (apply str)))

;; chart of the relative frequencies of first letters in the cleartext and cryptogram
(barchart (pct-frequencies (firstletters bunchotext)))
(barchart (pct-frequencies (firstletters bunchotextcryp)))

;; discover the unmapping function by using the keys from each sorted map
(def histocrypfreq (pct-frequencies (ci-lettersonly bunchotextcryp)))
(def histofreq (pct-frequencies (ci-lettersonly bunchotext)))
(def discovered-unmap (zipmap (map first histocrypfreq) (map first histofreq)))

(def sec "This is a simple message. Encrypt it and see what you get!")
(def hidden (crypt lettermap sec))
;;"e/[Y\"[Y\",\"Y[3stH\"3HYY,NH{\"]W8y-s?\"[?\",Wz\"YHH\"L/,?\"-n=\"NH?5"

(crypt discovered-unmap hidden)
;; "Ghis is a sipmle pessage. Wncrymt it and see what you get!"
