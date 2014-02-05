(ns clothereum.protocol-test
  (:use [gloss.io :only [encode decode]]
        [clothereum.rlp :only [bi->bs bs->bi bs->s s->bs hexify]]
        gloss.data.bytes.core
        clothereum.protocol
        clojure.test))

   ;0x22400891000000088400000043414243
(def test-pairs
  {
   {:type :hello :protocol-version 0 :network-id 0 :client-id "ABC" :capabilities 0 :listen-port 0} 0x000000434142430000
   {:type :disconnect :reason :disconnect-requested} 0x0100
   {:type :ping } 0x02
   {:type :pong } 0x03
   {:type :get-peers } 0x10
   {:type :peers } 0x11
   {:type :transactions } 0x12
   {:type :blocks } 0x13
   {:type :get-chain } 0x14
   {:type :not-in-chain } 0x15
   {:type :get-transactions } 0x16
  })

(deftest packet-encoding
  (testing "encode packet"
    (is {:magic :ethereum :size 1 :payload {:type :ping}}
        (decode packet (bi->bs 0x224008910000000102)))))

(deftest decoding
  (doall
    (for [[e d] test-pairs]
      (testing (str "decoding " (prn-str e))
        (is (= e (decode payload (bi->bs d))))))))

(deftest encoding
  (doall
    (for [[e d] test-pairs]
      (testing (str "encoding " (prn-str e))
        (is (= d (bs->bi (encode payload e))))))))


;(map #(format "%x" %) (.getBytes (encode-rlp 0x20100102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F)))
