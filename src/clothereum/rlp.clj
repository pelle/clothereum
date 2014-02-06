;; http://wiki.ethereum.org/index.php/RLP
(ns clothereum.rlp
  (:use gloss.core
        gloss.io
        gloss.data.primitives
        clojure.math.numeric-tower)
  (:require [gloss.core.protocols :as proto]
            [gloss.data.bytes :as bytes])
  (:import [gloss.core.protocols Reader Writer]))

(def LARGEINT (expt 2 256))

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


(defn bi->bs [s]
  (gloss.io/to-buf-seq (.toByteArray (biginteger s))))

(defn s->bs [s]
  (gloss.io/to-buf-seq (.getBytes s "ISO-8859-1")))

(defn bs->s [bs]
  (String. (.array (contiguous bs)) "ISO-8859-1"))

(defn bs->bi [bs]
  (BigInteger. (.array (contiguous bs))))

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

                 (cond
                     (number? val)
                     (if (< val 24)
                       (proto/with-buffer [buf 1]
                                          (proto/write-bytes (primitive-codecs :ubyte) buf val))
                       (let [bi (biginteger val)
                             data (.toByteArray bi )]
                         (if (< bi LARGEINT)
                           (proto/with-buffer [buf (inc (count data))]
                                              (.put buf (byte (+ 23 (count data))))
                                              (.put buf data))
                           (let [size-bytes (.toByteArray (biginteger (count data)))]
                             (proto/with-buffer [buf (+ 1 (count size-bytes) (count data))]
                                              (.put buf (byte (+ 55 (count size-bytes))))
                                              (.put buf size-bytes)
                                              (.put buf data))))))

                    (string? val)
                      (let [size (count val)
                            tiny (< size 56)]
                        (proto/with-buffer [buf (if tiny (inc size) (+ 2 size))]
                                           (if tiny
                                             (proto/write-bytes (primitive-codecs :ubyte) buf (+ size 64))
                                             (do
                                               (proto/write-bytes (primitive-codecs :ubyte) buf 120) ; TODO handle cases where size greater than a byte
                                               (proto/write-bytes (primitive-codecs :ubyte) buf size)))
                                           (.put buf (.getBytes val "ISO-8859-1"))))

                        (sequential? val)
                        (let [size (count val)]
                          (concat
                           (if (< size 56)
                             (proto/write-bytes (primitive-codecs :ubyte) buf (+ 128 size))
                             (concat
                               ;; TODO This only handles cases where number is < 256 bytes
                               (proto/write-bytes (primitive-codecs :ubyte) buf (+ 128 1))
                               (proto/write-bytes (primitive-codecs :ubyte) buf size)
                               ))
                           (apply concat
                            (for [c val]
                              (proto/write-bytes rlp buf c)))))))))

(defn decode-rlp [s]
  (decode rlp (s->bs s)))

(defn encode-rlp [d]
  (bs->s (encode rlp d)))

(defn hexify [s]
  (map #(format "%x" %) (.getBytes s)))
