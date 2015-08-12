(define-library (scheme lazy)
  (export
    (rename ##make-promise make-promise)
    (rename ##promise? promise?)
    ;; Gambit's delay implementation has same semantic
    (rename delay delay-force)
    delay force)
  (import (scheme base)
          (only (gambit) force)
          (primitives ##make-promise ##promise?))
  (begin
    (define-syntax delay
      (syntax-rules ()
        ((delay expression)
         (##make-promise (lambda () expression))))))
)
