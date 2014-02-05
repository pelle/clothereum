;; http://wiki.ethereum.org/index.php/RLP
(ns clothereum.protocol
  (:use gloss.core
        gloss.io
        clothereum.rlp))

(defcodec magic (enum :uint32 {:ethereum 0x22400891}))

(defcodec payload-type (enum :ubyte
                             { :hello 0x00
                               :disconnect 0x01
                               :ping 0x02
                               :pong 0x03
                               :get-peers 0x10
                               :peers 0x11
                               :transactions 0x12
                               :blocks 0x13
                               :get-chain 0x14
                               :not-in-chain 0x15
                               :get-transactions 0x16}) )


(defcodec disconnect-reason (enum :ubyte :disconnect-requested :tcp-sub-system-error :bad-protocol :useless-peer :too-many-peers))

(defcodec hello (ordered-map :type :hello :protocol-version rlp :network-id rlp :client-id rlp :capabilities rlp :listen-port rlp ))
(defcodec disconnect (ordered-map :type :disconnect :reason disconnect-reason))
(defcodec ping (ordered-map :type :ping))
(defcodec pong (ordered-map :type :pong))
(defcodec get-peers (ordered-map :type :get-peers))
(defcodec peers (ordered-map :type :peers)) ;; TODO [0x11, [IP1, Port1], [IP2, Port2], ... ]
(defcodec transactions (ordered-map :type :transactions)) ;; TODO [0x12, [nonce, receiving_address, value, ... ], ... ]
(defcodec blocks (ordered-map :type :blocks)) ;; TODO [0x13, [block_header, transaction_list, uncle_list], ... ]
(defcodec get-chain (ordered-map :type :get-chain)) ;; TODO [0x14, Parent1, Parent2, ..., ParentN, Count] Are Parents implemented as RLP array
(defcodec not-in-chain (ordered-map :type :not-in-chain )) ;; TODO [0x15, Hash] add :hash rlp (but what is encoding of hash?)
(defcodec get-transactions (ordered-map :type :get-transactions))

(defcodec payload
  (header
    payload-type
    {:hello hello
         :disconnect disconnect
         :ping ping
         :pong pong
         :get-peers get-peers
         :peers peers
         :transactions transactions
         :blocks blocks
         :get-chain get-chain
         :not-in-chain not-in-chain
         :get-transactions get-transactions }
    :type))

(defcodec packet (ordered-map :magic magic :size :uint32 :payload payload))

(defn ->hex [codec data]
  (hexify (bs->s (encode codec data))))


;(hexify (bs->s (encode payload {:type :disconnect :reason :useless-peer})))

;(->hex payload {:type :hello :protocol-version 0 :network-id 0 :client-id "ABC" :capabilities 0 :listen-port 0})

;(decode payload-type (clothereum.rlp/bi->bs 0x00))
;(decode payload (clothereum.rlp/s->bs ))