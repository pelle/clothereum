(ns clothereum.rlp-test
  (:use [gloss.io :only [encode decode]]
        gloss.data.bytes.core)
  (:require [clojure.test :refer :all]
            [clothereum.rlp :refer :all]))

(def test-pairs
  { "dog" "\u0043dog"
   ["cat" "dog"] "\u0082\0043cat\0043dog"
   "" "\u0040"
   [] "\u0080"
   15 "\u000f"
   1024 "\u0042\u0004\u0000"
   [ [] [[]] [ [] [[]] ] ] "\u0083\u0080\u0081\u0080\u0082\u0080\u0081\u0080"}
  )

;(.get (first (create-buf-seq (gloss.io/to-byte-buffer "\u0080"))) 1 )

;(encode rlp [])

(deftest decoding
  (doall
    (for [[e d] test-pairs]
      (do
        (println (str "TESTING: " (prn-str e)))
        (testing (prn-str e)
          (is (= e (decode rlp (create-buf-seq (gloss.io/to-byte-buffer (.getBytes d "ISO-8859-1")))))))))))

(defn decode-string [b]
  (try
    (decode rlp (create-buf-seq (gloss.io/to-byte-buffer (.getBytes b "ISO-8859-1"))))
    (catch Exception e nil)))

(decode-string "\u0080")

;(.get (gloss.io/to-byte-buffer (.getBytes "\u0080" "ISO-8859-1")) 1)
(= (hex 0x80) (hex (unchecked-byte (.get (.duplicate (first (create-buf-seq (gloss.io/to-byte-buffer (.getBytes "\u0080" "ISO-8859-1")))))))))
(unchecked-byte 0x80)
(map #(decode-string (last %)) test-pairs)

(deftest encoding
  (testing "Test data"
    (for [[e d] test-pairs]
      (is (= d (encode rlp e))))))


