;; -*- mode: scheme -*-

(library (Data.Show foreign)
  (export showIntImpl
          showNumberImpl
          showCharImpl
          showStringImpl
          showArrayImpl
          (rename [consImpl cons])
          join)
  (import (chezscheme)
          (prefix (purs runtime) rt:)
          (only (purs runtime bytestring) bytestring
                                          number->bytestring
                                          string->bytestring
                                          bytestring-uncons-code-unit
                                          bytestring->string
                                          bytestring-append
                                          bytestring-make-regex
                                          bytestring-regex-replace-all)
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
      (let ([regex (bytestring-make-regex (string->bytestring "[\\x00-\\x1F\\x7F\"]"))]
            [replacement (lambda (match)
                           (let-values ([(c _) (bytestring-uncons-code-unit match)])
                             (cond
                               [(char=? c #\") (bytestring #\\ c)]
                               [(char=? c #\\) (bytestring #\\ c)]
                               [(char=? c #\alarm) (bytestring #\\ #\a)]
                               [(char=? c #\backspace) (bytestring #\\ #\b)]
                               [(char=? c #\page) (bytestring #\\ #\f)]
                               [(char=? c #\newline) (bytestring #\\ #\n)]
                               [(char=? c #\return) (bytestring #\\ #\r)]
                               [(char=? c #\tab) (bytestring #\\ #\t)]
                               [(char=? c #\vtab) (bytestring #\\ #\v)]
                               [else c])))])
        (bytestring-append
          (bytestring #\")
          (bytestring-append
            (bytestring-regex-replace-all regex s replacement)
            (bytestring #\"))))))

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

  (define consImpl
    (lambda (head)
      (lambda (tail)
        (srfi:214:flexvector-append (rt:make-array head) tail))))

  (define join
    (lambda (separator)
      (lambda (xs)
        (string-join xs separator))))

)
