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

(defn ->ba
  "If possible converts value to bytearray. If not returns falsy"
  [v]
  (cond
   (string? v)
   (.getBytes v "ISO-8859-1")
   (number? v)
   (.toByteArray (biginteger v))))

(defn read-byte-array [b l]
  (let [ba (byte-array l)]
    (.get (.first b) ba)
    ba))

(defn read-big-integer [b l]
  (BigInteger. (read-byte-array b l)))

(defn read-string [b l]
  (String. (read-byte-array b l) "ISO-8859-1"))

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
            ;; Byte Arrays
           (< h 0x80)
             [true h b]
           (< h 0xb7)
             (let [l (- h 0x80)]
               (if (< (byte-count b) l)
                 [false rlp b]
                 [true (read-byte-array b l) (gloss.data.bytes/drop-bytes b l)]))
           (< h 192)
             (let [ll (- h 0xb7)]
               (if (< (byte-count b) ll)
                 [false rlp b]
                 (let [l (read-big-integer b ll)]
                   (if (< (byte-count b) l)
                     [false rlp b]
                     [true (read-byte-array b l) (gloss.data.bytes/drop-bytes b (+ l ll))]))))
           ;; Lists
           (= h 0xc0)
              [true [] b]

           (< h 0xf7)
             (let [l (- h 0xc0)]
                (proto/read-bytes (fixed-length-codec rlp l) b))

           :else
             (let [ll (- h 0xf7)]
               (if (< (byte-count b) ll)
                 [false rlp b]
                 (let [l (read-big-integer b ll)]
                   (proto/read-bytes (fixed-length-codec rlp l) (gloss.data.bytes/drop-bytes b ll))))))))
      Writer
      (sizeof [_] nil)
      (write-bytes [_ buf val]
                   (if-let [ba (->ba val)]
                      (let [size (count val)]
                        (if
                         (and (= size 1)
                              (< (long (first ba)) 0x80))
                           (proto/with-buffer [buf 1]
                             (.put buf ba))
                         (if (< size 56)
                           (proto/with-buffer [buf (inc size)]
                                              (proto/write-bytes (primitive-codecs :ubyte) (+ 0x80 size))
                                              (.put buf ba))
                           (let [size-ba (->ba size)]
                             (proto/with-buffer [buf (+ 1 (count size-ba) size)]
                                              (proto/write-bytes (primitive-codecs :ubyte) (+ 0xb7 (count size-ba)))
                                              (.put buf size-ba)
                                              (.put buf ba))))))
                     (if (sequential? val)
                       (let [size (count val)]
                         (concat
                          (if (< size 56)
                            (proto/write-bytes (primitive-codecs :ubyte) buf (+ 0xc0 size))
                            (let [size-ba (->ba size)]
                              (proto/with-buffer [buf (inc (count size-ba))]
                                 (proto/write-bytes (primitive-codecs :ubyte) buf (+ 0xf7 (count size-ba)))
                                 (.put buf size-ba))))
                          (apply concat
                                 (for [c val]
                                   (proto/write-bytes rlp buf c))))))))))

(defn decode-rlp [s]
  (decode rlp (s->bs s)))

(defn encode-rlp [d]
  (bs->s (encode rlp d)))

(defn hexify [s]
  (map #(format "%x" %) (.getBytes s)))
