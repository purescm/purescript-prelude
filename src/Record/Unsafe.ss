;; -*- mode: scheme -*-

(library (Record.Unsafe foreign)
  (export unsafeHas
          unsafeGet
          unsafeSet
          unsafeDelete)
  (import (chezscheme)
          (only (purs runtime bytestring) bytestring->symbol)
          (prefix (purs runtime) rt:))

  (define unsafeHas
    (lambda (label)
      (lambda (rec)
        (symbol-hashtable-contains? rec (bytestring->symbol label)))))

  (define unsafeGet
    (lambda (label)
      (lambda (rec)
        (rt:object-ref rec (bytestring->symbol label)))))

  (define unsafeSet
    (lambda (label)
      (lambda (value)
        (lambda (rec)
          (let ([rec-copy (rt:object-copy rec)])
            (rt:object-set! rec-copy (bytestring->symbol label) value)
            rec-copy)))))

  (define unsafeDelete
    (lambda (label)
      (lambda (rec)
        (let ([rec-copy (rt:object-copy rec)])
          (symbol-hashtable-delete! rec-copy (bytestring->symbol label))
          rec-copy))))

)
