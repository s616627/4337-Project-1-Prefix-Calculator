#lang racket

;;function to find what mode will be used
(define interactive?
   (let [(args (current-command-line-arguments))]
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))

;;function to display error message
(define (error-msg msg)
  (when interactive?
      (display "Error: "))
  (displayln msg))

;;function to display result
(define (display-result id value)
  (display id)
  (display ": ")
  (display(real->double-flonum value))
  (newline))

;;operator hash table
(define operator-table
  (hash
   "+"      (lambda (a b) (+ a b))
   "-"      (lambda (a b) (- a b))
   "*"      (lambda (a b) (* a b))
   "/"      (lambda (a b) (/ a b))
   "square" (lambda (a) (* a a))
   "cube"   (lambda (a) (* a a a))
   "pow"    (lambda (a b) (expt a b))
   "mod"    (lambda (a b) (remainder a b))))

;;evaluator
(define (eval-expr tokens history))
  (cond
    [(null? tokens) (error "unexcpected end of input")]

    ;;for the operator
    [(member (firts-tokens) (hash-keys operator-table))
     (define op (first tokens))
     (define proc (hash-ref operator-table op))
     (cond
          ;;unary operator "-"
          [(equal? op "-")
           (define-values (a rest1) (eval-expr (rest tokens) history))
           (values (proc a) rest1)]
       ;; binary operators
          [else
           (define-values (a rest1) (eval-expr (rest tokens) history))
           (define-values (b rest2) (eval-expr rest1 history))
           (values (proc a b) rest2)])]
    
    ;; history reference
    [(regexp-match #px"^\\$(\\d+)$" (first tokens))
     => (lambda (m)
          (define idx (string->number (second m)))
          (if (and (positive? idx) (<= idx (length history)))
              (values (list-ref (reverse history) (- idx 1)) (rest tokens))
              (error "history reference out of range")))]

        ;; numeric literal
    [(string->number (first tokens))
     => (lambda (n) (values n (rest tokens)))]

    ;; anything else returns an error
    [else (error "invalid token")]))

;;REPL (main calculator loop)
(define (calculator)
  ;;loop of getting inputs
  (define (loop history)
    
    (when interactive? (display ">") (flush-output))
    
    ;;get input
    (define input (read-line))
    
    ;;split input string
    ;;(define tokens (string-split input))

    ;;if 'quit' is inputed, quit program
    (cond
      [(eof-object? input) (void)] ;; end-of-file ends batch mode
      [(string=? input "quit")
       (when interactive? (displayln "Goodbye")) (void)]

      [else
       (define tokens (filter (lambda (s) (not (string-blank? s))) (string-split input)))
       (cond
         [(null? tokens) (error-msg "Empty expression") (loop history)]
         [else
          (with-handlers ([exn:fail? (lambda (e) (error-msg (exn-message e)) (loop history))])
            (define-values (result rest) (eval-expr tokens history))
            (if (null? rest)
                (begin
                  (define new-history (cons result history))
                  (display-result (length new-history) result)
                  (loop new-history))
                (begin (error-msg "Leftover input after expression") (loop history))))])]))

  ;; start
  (loop '()))

;; --- Run ---
(calculator)