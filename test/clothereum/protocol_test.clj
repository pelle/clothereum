(ns clothereum.protocol-test
  (:use [gloss.io :only [encode decode]]
        [clothereum.rlp :only [bi->bs bs->bi bs->s s->bs hexify]]
        gloss.data.bytes.core
        clothereum.protocol
        clojure.test))

   ;0x22400891000000088400000043414243
(def test-pairs
  {
   {:type :hello :protocol-version 0 :network-id 0 :client-id "ABC" :capabilities 0 :listen-port 0} "\u0000\u0000\u0000\u0043ABC\u0000\u0000"
   {:type :disconnect :reason :disconnect-requested} "\u0001\u0000"
   {:type :ping } "\u0002"
   {:type :pong } "\u0003"
   {:type :get-peers } "\u0010"
   {:type :peers } "\u0011"
   {:type :transactions } "\u0012"
   {:type :blocks } "\u0013"
   {:type :get-chain } "\u0014"
   {:type :not-in-chain } "\u0015"
   {:type :get-transactions } "\u0016"
  })

(deftest packet-encoding
  (testing "encode packet"
    (is {:magic :ethereum :size 1 :payload {:type :ping}}
        (decode packet (bi->bs 0x224008910000000102)))))

(deftest decoding
  (doall
    (for [[e d] test-pairs]
      (testing (str "decoding " (prn-str e))
        (is (= e (decode payload (s->bs d))))))))

(deftest encoding
  (doall
    (for [[e d] test-pairs]
      (testing (str "encoding " (prn-str e))
        (is (= d (bs->s (encode payload e))))))))


;(map #(format "%x" %) (.getBytes (encode-rlp 0x20100102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F)))
