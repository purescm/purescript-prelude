;; -*- mode: scheme -*-

(library (Data.Show foreign)
  (export showIntImpl
          showNumberImpl
          showCharImpl
          showStringImpl
          showArrayImpl
          cons
          join)
  (import (only (rnrs base) define lambda let + = cond else if)
          (only (chezscheme) format)
          (prefix (purs runtime lib) rt:)
          (only (purs runtime bytestring) number->bytestring string->bytestring bytestring->string bytestring-append)
          (prefix (purs runtime srfi :214) srfi:214:))

  (define showIntImpl
    (lambda (n)
      (number->bytestring n)))

  (define showNumberImpl
    (lambda (n)
      (number->bytestring n)))

  (define showCharImpl
    (lambda (c)
      (string->bytestring (format "~s" c))))

  (define showStringImpl
    (lambda (s)
      (string->bytestring (format "~s" (bytestring->string s)))))

  (define (string-join xs separator)
    (let ([len (rt:array-length xs)])
      (cond
        [(= len 0) (string->bytestring "")]
        [(= len 1) (rt:array-ref xs 0)]
        (else
          (let recur ([i 1]
                      [buffer (rt:array-ref xs 0)])
            (if (= len i)
              buffer
              (recur (+ i 1) (bytestring-append buffer (bytestring-append separator (rt:array-ref xs i))))))))))

  (define showArrayImpl
    (lambda (f)
      (lambda (xs)
        (bytestring-append
          (string->bytestring "[")
          (bytestring-append (string-join (srfi:214:flexvector-map f xs) (string->bytestring ","))
                             (string->bytestring "]"))))))

  (define cons
    (lambda (head)
      (lambda (tail)
        (srfi:214:flexvector-append (rt:make-array head) tail))))

  (define join
    (lambda (separator)
      (lambda (xs)
        (string-join xs separator))))

)
