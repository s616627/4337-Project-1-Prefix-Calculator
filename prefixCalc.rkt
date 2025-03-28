#lang racket

;;use prompt? to find what mode will be used
(define prompt?
   (let [(args (current-command-line-arguments))]
     (cond
       [(= (vector-length args) 0) #t]
       [(string=? (vector-ref args 0) "-b") #f]
       [(string=? (vector-ref args 0) "--batch") #f]
       [else #t])))

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

;;REPL
(define (calculator)
  ;;loop of getting inputs
  (define (loop)
    (display ">") (flush-output)
    
    ;;get input
    (define input (read-line))
    
    ;;split input string
    (define tokens (string-split input))

    ;;if 'quit' is inputed, quit program
    (cond
      [(or (null? tokens) (string=? (first tokens) "quit"))
       (displayln "Goodbye")]

      ;;otherwise
      [else
       ;; get operation (first character)
       (define op (first tokens))
       ;;get numbers from expression (parse rest of input)
       (define args (map string->number (rest tokens)))
       (cond
         ;;if an operator cant be found using the hashtable, print out an error message
         [(not (hash-has-key? operator-table op))
          (displayln "Unknown operator.") (loop)]
         [else
          ;;handle errors
          (define proc (hash-ref operator-table op))
          (with-handlers ([exn:fail? (lambda (e)
                                       (displayln "Error during calculation.")
                                       (loop))]) ;; redo loop
            ;;calculate and display the answer
            (define result (apply proc args))
            (displayln result)
            ;;redo loop
            (loop))])]))
  ;;redo loop
  (loop))

;;does the program
(calculator)
