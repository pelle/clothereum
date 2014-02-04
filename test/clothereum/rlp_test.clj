(ns clothereum.rlp-test
  (:use [gloss.io :only [encode decode]]
        gloss.data.bytes.core)
  (:require [clojure.test :refer :all]
            [clothereum.rlp :refer :all]))

(def test-pairs
  {
   15 "\u000f"
   69 "\u0018\u0045"
   257 "\u0019\u0001\u0001"
   0x20100102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F
   "\u0038\u0021\u0020\u0010\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000a\u000b\u000c\u000d\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f"

   "" "\u0040"
   "dog" "\u0043dog"
   "Lorem ipsum dolor sit amet, consectetur adipisicing elit" "\u0078\u0038Lorem ipsum dolor sit amet, consectetur adipisicing elit"

   [] "\u0080"
   [15 "dog"] "\u0082\u000f\u0043dog"
   ["cat" "dog"] "\u0082\u0043cat\u0043dog"
   [ [] [[]] [ [] [[]] ] ] "\u0083\u0080\u0081\u0080\u0082\u0080\u0081\u0080"}
  )

(deftest decoding
  (doall
    (for [[e d] test-pairs]
      (do
        (testing (str "decoding " " -> " (prn-str e))
          (is (= e (decode-rlp d))))))))

(deftest encoding
  (testing "Test data"
    (if false
      (for [[e d] test-pairs]
        (is (= d (encode-rlp e)))))))



;(bs->s (s->bs (test-pairs "dog")))
