;; -*- mode: scheme -*-

(library (Data.Show.Generic foreign)
  (export intercalate)
  (import (only (rnrs base) define lambda if let cond else = +)
          (only (purs runtime bytestring) string->bytestring bytestring-append)
          (prefix (purs runtime lib) rt:))

  (define intercalate
    (lambda (separator)
      (lambda (xs)
        (let ([len (rt:array-length xs)])
          (cond
            [(= len 0) (string->bytestring "")]
            [(= len 1) (rt:array-ref xs 0)]
            (else
              (let recur ([i 1]
                          [buffer (rt:array-ref xs 0)])
                (if (= len i)
                  buffer
                  (recur (+ i 1) (bytestring-append buffer (bytestring-append separator (rt:array-ref xs i))))))))))))

)
