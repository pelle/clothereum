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

;(declare rlp)

(defn hex [x]
  (format "%x" x))

(defn fixed-length-codec
  [codec n]
  (compile-frame (vec (take n (repeat codec)))))

(defn read-big-integer [b l]
  (let [ba (byte-array l)]
    (.get (.first b) ba)
    (BigInteger. ba)))

(defn s->bs [s]
  (gloss.io/to-buf-seq (.getBytes s "ISO-8859-1")))

(let [b (s->bs "1")]
;  (.get (first b))
  (gloss.data.bytes/take-contiguous-bytes b 1)
  )

(defn bs->s [bs]
  (String. (.array (contiguous bs))))

(def rlp
  (reify
     Reader
     (read-bytes [_ b]
       (let [h (decode (primitive-codecs :ubyte) b false)
             b (gloss.data.bytes/drop-bytes b 1)]
         (cond
            ;; Numbers
           (< h 24)
             [true h b]
           (< h 56)
             (let [l (- h 23)]
               (if (< (byte-count b) l)
                 [false rlp b]
                 [true (read-big-integer b l) (gloss.data.bytes/drop-bytes b l)]))
           (< h 64)
             (let [ll (- h 55)]
               (if (< (byte-count b) ll)
                 [false rlp b]
                 (let [l (read-big-integer b ll)]
                   (if (< (byte-count b) l)
                     [false rlp b]
                     [true (read-big-integer b l) (gloss.data.bytes/drop-bytes b (+ l ll))]))))
            ;; Strings
           (= h 64)
             [true "" b]

           (< h 120)
             (let [l (- h 64)]
                (proto/read-bytes (string :ISO-8859-1 :length l) b))

           (< h 128)
             (let [l (- h 119)]
               (if (< (byte-count b) l)
                 [false rlp b]
                 (let [l (read-big-integer b l)]
                   (proto/read-bytes (string :ISO-8859-1 :length (inc l)) b))))

           ;; Arrays
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

(defn decode-rlp [s]
  (decode rlp (s->bs s)))

(defn encode-rlp [d]
  (bs->s (encode rlp d)))

