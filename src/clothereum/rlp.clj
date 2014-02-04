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

(declare rlp)
(declare read-rlp)

(defn hex [x]
  (format "%x" x))

(defn fixed-length-codec
  [codec n]
  (compile-frame (vec (take n (repeat codec)))))

(defn read-big-integer [b l]
  (let [ba (byte-array l)]
    (.get (.first b) ba)
    (BigInteger. ba)))

(def rlp
  (reify
     Reader
     (read-bytes [_ b]
       (let [;b  (contiguous b)
             h (decode (primitive-codecs :ubyte) b false)
             b (gloss.data.bytes/drop-bytes b 1)]
;         (println "h = " (format "%x" h))
         (cond
           (< h 24)
             [true h b]
           (< h 56)
             (let [l (- h 23)]
               (if (< (byte-count b) l)
                 [false rlp b]
                 [true (read-big-integer b l) (gloss.data.bytes/drop-bytes b l)]))
           (< h 64)
             (let [l (- h 55)]
               (if (< (byte-count b) l)
                 [false rlp b]
                 (let [l (read-big-integer b l)]
                   (if (< (byte-count b) l)
                     [false rlp b]
                     [true (read-big-integer b l) b]))))

           (= h 64)
             [true "" b]

           (< h 120)
             (let [l (- h 64)]
                (proto/read-bytes (string :ascii :length l) b))

           (< h 128)
             (let [l (- h 119)]
               (if (< (byte-count b) l)
                 [false rlp b]
                 (let [l (read-big-integer b l)]
                   (proto/read-bytes (string :ascii :length (inc l)) b))))

           (= h 128)
              [true [] b]

           (< h 184)
             (let [l (- h 128)]
                (proto/read-bytes (fixed-length-codec rlp l) b))

           (< h 192)
             (let [ll (- h 183)]
               (if (< (byte-count b) ll)
                 [false rlp b]
                 (let [l (read-big-integer b ll)]
                   (proto/read-bytes (fixed-length-codec rlp l) (gloss.data.bytes/drop-bytes b ll))))))))
      Writer
      (sizeof [_] nil)
      (write-bytes [_ buf val]
                   [])))

(defn read-rlp [b]
  (proto/read-bytes rlp b))


