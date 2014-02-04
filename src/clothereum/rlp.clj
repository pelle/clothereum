;; http://wiki.ethereum.org/index.php/RLP
(ns clothereum.rlp
  (:use gloss.core
        gloss.io
        gloss.data.primitives)
  (:require [gloss.core.protocols :as proto]
            [gloss.data.bytes :as bytes])
  (:import [gloss.core.protocols Reader Writer]))


(defn unsigned-byte
  "Stupid ass hack to read the value of an unsigned byte"
  [b]
  (let [ba (byte-array 2)]
    (aset-byte ba 1 (unchecked-byte b))
    (BigInteger. ba)))

(defn u<
  "unsigned less than"
  [b1 b2]
  (< (unsigned-byte b1) (unsigned-byte b2)))

(defn c->b [c]
  (byte-array [(unchecked-byte c)]))

(defn c->s [c] (java.lang.String. (c->b c)))

(declare read-rlp)

(defn hex [x]
  (format "%x" x))

(def rlp
  (reify
     Reader
     (read-bytes [_ b]
       (let [h (unsigned-byte (.get (.duplicate (first b))))]
         (println "h = " (format "%x" h))
         (cond
           (and ;(>= 0 h)
               (< h (unsigned-byte 0x40)))
             (proto/read-bytes (primitive-codecs :byte) b)
           (= h (unsigned-byte 0x40))
             [true "" (gloss.data.bytes/drop-bytes b 1)]
           (< h (unsigned-byte 0x77))
             (let [l (unsigned-byte (- h (unsigned-byte 0x40)))]
                (proto/read-bytes (string :ascii :length l) (gloss.data.bytes/drop-bytes b 1)))
           (< h (unsigned-byte 0x80))
             (let [sl (- h (unsigned-byte 0x77))
                   _ (println (str "sl = " sl))
                   l (get 1 (proto/read-bytes (primitive-codecs (condp = sl
                                                                  1 :byte
                                                                  2 :uint16-be
                                                                  4 :uint32-be)) (gloss.data.bytes/drop-bytes b 1)))]
                (proto/read-bytes (string :ascii :length l) (gloss.data.bytes/drop-bytes b (inc l))))
           (= (unsigned-byte h) (unsigned-byte 0x80))
          (do
            [true [] (gloss.data.bytes/drop-bytes b 1)])
           (< h (unsigned-byte 0xb7))
          (let [l (unsigned-byte (- h (unsigned-byte 0x80)))]
            (proto/read-bytes (compile-frame (vec (take l (repeat :read-rlp)))) b))
           :else
          (let [sl (unsigned-byte (- h (unsigned-byte 0x77)))
                   l (get 1 (proto/read-bytes (primitive-codecs (condp = sl
                                                                  1 :byte
                                                                  2 :uint16-be
                                                                  4 :uint32-be)) (gloss.data.bytes/drop-bytes b 1)))]
            (proto/read-bytes (compile-frame (vec (take l (repeat :read-rlp)))) (gloss.data.bytes/drop-bytes b (inc l)))))))
      Writer
      (sizeof [_] nil)
      (write-bytes [_ buf val]
                   [])))

(defn read-rlp [b]
  (proto/read-bytes rlp b))


