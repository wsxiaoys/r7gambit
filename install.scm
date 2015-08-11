#!/usr/bin/env gsc-script -i

(define (bootstrap)
  (load "~~lib/syntax-case")
  (load "compat-gambit.scm")
  (load "runtime.scm")
  (load "expander.scm")
  (ex:expand-file "standard-libraries.scm" "standard-libraries.exp")
  (ex:expand-r5rs-file "expander.scm" "expander.exp"
                       (ex:environment '(rnrs base))))

(define (make-expander)
  (lambda (x)
    (let* ((exp (##desourcify x))
           (expanded (car (ex:expand-r7rs (list exp))))
           (result (##sourcify expanded x)))
      result)))

(define (install)
  (load "compat-gambit.scm")
  (load "runtime.scm")
  (load "standard-libraries.exp")
  (load "expander.exp")
  (set! ##expand-source (make-expander))
  (set! c#expand-source (make-expander))
  (pp "Install done")
  (##repl-debug-main))

(define (main arg)
  (let ((cmd (string->symbol arg)))
    (case cmd
      ((bootstrap) (bootstrap))
      ((install) (install))
      (else (error "Invalid argument" cmd)))))

(main (cadr (command-line)))
