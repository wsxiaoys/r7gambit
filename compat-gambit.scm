;;;===============================================================================
;;;
;;; MzScheme compatibility file:
;;;
;;; Uncomment the appropriate LOAD command in macros-core.scm
;;;
;;;===============================================================================

;; A numeric string that uniquely identifies this run in the universe

; (define (ex:unique-token) (number->string (time->seconds (current-time))))
(define (ex:unique-token) 31)

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;; Just give this damn thing a binding

(define assertion-violation
  (lambda args
    (display 'assertion-violation)
    (newline)
    (display args)
    (newline)
    (car #f)))

(define pretty-print write)

;; These are only partial implementations for specific use cases needed.
;; Full implementations should be provided by host implementation.

(define (memp proc ls)
  (cond ((null? ls) #f)
        ((pair? ls) (if (proc (car ls))
                        ls
                        (memp proc (cdr ls))))
        (else (assertion-violation 'memp "Invalid argument" ls))))

(define (filter p? lst)
  (if (null? lst)
      '()
      (if (p? (car lst))
          (cons (car lst)
                (filter p? (cdr lst)))
          (filter p? (cdr lst)))))

(define (for-all proc l . ls)
  (or (null? l)
      (and (apply proc (car l) (map car ls))
           (apply for-all proc (cdr l) (map cdr ls)))))

;; Only the most minimal extremely partial implementation
;; of  r6rs records as needed for our specific use cases.
;; Note that most arguments are ignored.

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (list name))

(define (make-record-constructor-descriptor rtd parent-constructor-descriptor protocol)
  rtd)

(define (record-accessor rtd k)
  (lambda (r) (vector-ref r (+ k 2))))

(define record-constructor #f)
(define record-predicate   #f)

(let ((record-tag (list 'record)))

  (set! record-constructor
        (lambda (cd)
          (lambda args
            (apply vector record-tag cd args))))

  (set! record-predicate
        (lambda (rtd)
          (let ((real-vector? ##vector?))
            (lambda (x)
              (and (real-vector? x)
                   (eq? (vector-ref x 0) record-tag)
                   (eq? (vector-ref x 1) rtd))))))

  (set! vector?
        (let ((real-vector? ##vector?))
          (lambda (x)
            (and (real-vector? x)
                 (not (eq? (vector-ref x 0) record-tag)))))))


