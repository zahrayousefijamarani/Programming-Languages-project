#lang racket

;requirements
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require (lib "eopl.ss" "eopl"))

;lexer
(define lang-lexer
  (lexer
   ("+" (token-plus))
   ("-" (token-minus))
   ("*" (token-mul))
   ("/" (token-div))
   ("**" (token-pow))
   ("<" (token-less))
   (">" (token-greater))
   ("==" (token-equal))
   ("=" (token-assign))
   ("," (token-comma))
   ("[" (token-brack-open))
   ("]" (token-brack-close))
   ("(" (token-paranth-open))
   (")" (token-paranth-close))
   ("True" (token-TRUE))
   ("False" (token-FALSE))
   ("None" (token-NONE))
   ("not" (token-not))
   ("or" (token-or))
   ("and" (token-and))
   (":" (token-colon))
   (";" (token-semicolon))
   ("for" (token-for))
   ("in" (token-in))
   ("break" (token-break))
   ("continue" (token-continue))
   ("if" (token-if)) 
   ("else" (token-else))
   ("def" (token-def))
   ("global" (token-global))
   ("return" (token-return))
   ("pass" (token-pass))
   ("print" (token-print))
   ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER (string->number lexeme)))
   ((::
     (:or (char-range #\a #\z) (char-range #\A #\Z) #\_)
     (:* (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_)))
    (token-ID lexeme))
   (whitespace (lang-lexer input-port))
   ((eof) (token-EOF))))

;tokens
(define-tokens token1 (NUMBER))
(define-tokens token2 (ID))
(define-empty-tokens token3 (
                             EOF
                             plus
                             minus
                             mul
                             div
                             pow
                             less
                             greater
                             equal
                             assign
                             comma
                             brack-open
                             brack-close
                             paranth-open
                             paranth-close
                             TRUE
                             FALSE
                             NONE
                             not
                             or
                             and
                             colon
                             semicolon
                             for
                             in
                             break
                             continue
                             if
                             else
                             def
                             global
                             return
                             pass
                             print
                             ))

;parser
(define lang-parser
  (parser
   (start statements)
   (end EOF)
   (error void)
   (tokens token1 token2 token3)
   (grammar
    (statements
     ((statement semicolon) (statements-exp (list $1)))
     ((statements statement semicolon) (statements-exp (append (exp->statements $1) (list $2)))))
    (statement
     ((compound-stmt) $1)
     ((simple-stmt) $1))
    (simple-stmt
     ((assignment) $1)
     ((return-stmt) $1)
     ((global-stmt) $1)
     ((print-stmt) $1)
     ((pass) (pass-exp))
     ((break) (break-exp))
     ((continue) (continue-exp)))
    (compound-stmt
     ((function-def) $1)
     ((if-stmt) $1)
     ((for-stmt) $1))
    (assignment
     ((ID assign expression) (assignment-exp $1 $3)))
    (return-stmt
     ((return) (return-stmt-exp (none-exp)))
     ((return expression) (return-stmt-exp $2)))
    (global-stmt
     ((global ID) (global-stmt-exp $2)))
    (print-stmt
     ((print paranth-open atom paranth-close) (print-stmt-exp $3)))
    (function-def
     ((def ID paranth-open params paranth-close colon statements) (function-def-exp $2 $4 $7))
     ((def ID paranth-open paranth-close colon statements) (function-def-exp $2 (params-exp null) $6)))
    (params
     ((param-with-default) (params-exp (list $1)))
     ((params comma param-with-default) (params-exp (append (exp->params $1) (list $3)))))
    (param-with-default
     ((ID assign expression) (list $1 $3)))
    (if-stmt
     ((if expression colon statements else-block) (if-stmt-exp $2 $4 $5)))
    (else-block
     ((else colon statements) $3))
    (for-stmt
     ((for ID in expression colon statements) (for-stmt-exp $2 $4 $6)))
    (expression
     ((disjunction) $1))
    (disjunction
     ((conjunction) $1)
     ((disjunction or conjunction) (or-exp $1 $3)))
    (conjunction
     ((inversion) $1)
     ((conjunction and inversion) (and-exp $1 $3)))
    (inversion
     ((not inversion) (not-exp $2))
     ((comparison) $1))
    (comparison
     ((sum compare-op-sum-pairs) (comparison-exp $1 $2))
     ((sum) $1))
    (compare-op-sum-pairs
     ((compare-op-sum-pair) (list $1))
     ((compare-op-sum-pairs compare-op-sum-pair) (append $1 (list $2))))
    (compare-op-sum-pair
     ((eq-sum) $1)
     ((lt-sum) $1)
     ((gt-sum) $1))
    (eq-sum
     ((equal sum) (list "==" $2)))
    (lt-sum
     ((less sum) (list "<" $2)))
    (gt-sum
     ((greater sum) (list ">" $2)))
    (sum
     ((sum plus term) (add-exp $1 $3))
     ((sum minus term) (sub-exp $1 $3))
     ((term) $1))
    (term
     ((term mul factor) (mul-exp $1 $3))
     ((term div factor) (div-exp $1 $3))
     ((factor) $1))
    (factor
     ((plus factor) (factor-exp "+" $2))
     ((minus factor) (factor-exp "-" $2))
     ((power) $1))
    (power
     ((atom pow factor) (power-exp $1 $3))
     ((primary) $1))
    (primary
     ((atom) $1)
     ((primary brack-open expression brack-close) (list-cell-exp $1 $3))
     ((primary paranth-open paranth-close) (call-exp $1 (arguments-exp null)))
     ((primary paranth-open arguments paranth-close) (call-exp $1 $3)))
    (arguments
     ((expression) (arguments-exp (list $1)))
     ((arguments comma expression) (arguments-exp (append (exp->arguments $1) (list $3)))))
    (atom
     ((ID) (var-exp $1))
     ((TRUE) (bool-exp #t))
     ((FALSE) (bool-exp #f))
     ((NONE) (none-exp))
     ((NUMBER) (num-exp $1))
     ((list) $1))
    (list
     ((brack-open expressions brack-close) (list-exp $2))
     ((brack-open brack-close) (list-exp null)))
    (expressions
     ((expressions comma expression) (append $1 (list $3)))
     ((expression) (list $1)))
    )))

;------------------------------------------------------
;evaluate function
(define evaluate
  (lambda (file-name)
    (define ns (make-base-namespace))

    (cond
      [(file-exists? file-name)
       (displayln (string-append "Test: " file-name))
       (define in (file->string file-name))
       (define lex-this (lambda (lexer input) (lambda () (lexer input))))
       (define my-lexer (lex-this lang-lexer (open-input-string in)))
       (let ((parser-res (lang-parser my-lexer)))
         (initialize-store!)
         (set! the-global-env (init-env #t))
         (set! the-scope-env (init-env #t))
         (value-of parser-res)
         (displayln ""))
       ]
      [else (displayln "File not found")]
      )
   )
 )

;------------------------------------------------------
;store: Each location may have arbitrary type of value.
(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define report-invalid-reference
  (lambda (ref)
    (eopl:error 'setref! "Invalid refrence: ~s" ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                (lambda (store1 ref1)
                  (cond
                    ((null? store1)
                     (report-invalid-reference ref))
                    ((zero? ref1)
                     (cons val (cdr store1)))
                    (else
                     (cons
                      (car store1)
                      (setref-inner
                       (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

;------------------------------------------------------
;environment: Accepts (string, expval) pairs.
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
     (eopl:error 'apply-env "Bad environment: ~s" env)))
  
(define-datatype environment env?
  (empty-env)
  (extend-env
    (var string?)
    (val expval?)
    (saved-env env?)))

(define apply-env
  (lambda (search-var env with-error)
    (cases environment env
      (empty-env ()
                 (if with-error
                     (report-no-binding-found search-var)
                     (void-val)))
      (extend-env (saved-var saved-val saved-env)
                  (if (equal? saved-var search-var)
                      saved-val
                      (apply-env search-var saved-env with-error)))
      (else
       (report-invalid-env env))
      )))

;doc: Used to initialize the-global-env
(define init-env
  (lambda (global)
          (extend-env "$global" (bool-val global) (empty-env))))

;doc: Used to add defined functions to environment to calling a function.
(define extend-env-with-functions
  (lambda (env)
    (let loop ([env env]
               [g-env the-global-env])
      (cases environment g-env
        (empty-env ()
                   env)
        (extend-env (var val saved-env)
                    (cases expval val
                      (ref-val (ref)
                               (let ([w (deref ref)])
                                 (if (expval? w)
                                     (cases expval w
                                       (proc-val (proc)
                                                 (cases procedure proc
                                                   (a-proc (ID params p-body)
                                                           (loop (extend-env ID val env) saved-env))))
                                       (else
                                        (loop env saved-env)))
                                     (loop env saved-env))))
                      (bool-val (bool) (loop env saved-env)) ;For $global in env with holds a bool-val
                      (else
                       (report-type-error)))))
      )))

;doc: Checks whether we are in global scope now
(define global-scope?
  (lambda (env)
    (expval->bool (apply-env "$global" env #t))))

;doc: Used for global variables and functions
(define the-global-env 'uninitialized)

;doc: Used for all known variables in a scope
(define the-scope-env 'uninitialized)

;------------------------------------------------------
;exp
(define-datatype exp exp?
  (statements-exp
   (statements-list list?))
  (assignment-exp
   (ID string?)
   (rhs exp?))
  (return-stmt-exp
   (exp1 exp?))
  (global-stmt-exp
   (ID string?))
  (print-stmt-exp
   (atom exp?))
  (pass-exp)
  (break-exp)
  (continue-exp)
  (function-def-exp
   (ID string?)
   (params exp?)
   (statements exp?))
  (if-stmt-exp
   (condition exp?)
   (true-statements exp?)
   (false-statements exp?))
  (for-stmt-exp
   (iterator-ID string?)
   (lst exp?)
   (statements exp?))
  (params-exp
   (exps-list list?))
  (or-exp
   (exp1 exp?)
   (exp2 exp?))
  (and-exp
   (exp1 exp?)
   (exp2 exp?))
  (not-exp
   (exp1 exp?))
  (comparison-exp
   (sum exp?)
   (compare-pairs list?))
  (add-exp
   (exp1 exp?)
   (exp2 exp?))
  (sub-exp
   (exp1 exp?)
   (exp2 exp?))
  (mul-exp
   (exp1 exp?)
   (exp2 exp?))
  (div-exp
   (exp1 exp?)
   (exp2 exp?))
  (factor-exp
   (sign string?)
   (exp1 exp?))
  (power-exp
   (exp1 exp?)
   (exp2 exp?))
  (call-exp
   (proc exp?)
   (arguments exp?))
  (list-cell-exp
   (lst exp?)
   (index exp?))
  (arguments-exp
   (exps-list list?))
  (var-exp
   (name string?))
  (list-exp
   (expressions list?))
  ;const exps
  (num-exp
   (num number?))
  (bool-exp
   (bool boolean?))
  (none-exp)
  (list-const-exp
   (values list?))
  ;thunk exp
  (thunk-exp
   (th thunk?))
  )

;exp extractors
(define exp->statements
  (lambda (exp1)
    (cases exp exp1
      (statements-exp (lst) lst)
      (else (report-type-mismatch 'statements-exp exp1)))))

(define exp->params
  (lambda (exp1)
    (cases exp exp1
      (params-exp (lst) lst)
      (else (report-type-mismatch 'params-exp exp1)))))

(define exp->arguments
  (lambda (exp1)
    (cases exp exp1
      (arguments-exp (lst) lst)
      (else (report-type-mismatch 'arguments-exp exp1)))))

;------------------------------------------------------
;value-of
(define value-of
  (lambda (exp1)
    (cases exp exp1 
      (statements-exp (lst)
                      (let loop ([stmt-list lst])
                        (if (null? stmt-list)
                            (void-val)
                            (let ([val1 (value-of (car stmt-list))])
                              (cases expval val1
                                (break-val () val1)
                                (continue-val () val1)
                                (return-val (ret-val) val1)
                                (else (loop (cdr stmt-list))))))))
      (assignment-exp (ID rhs)
                      (assign ID rhs))
      (return-stmt-exp (exp1)
                       (return-val (value-of exp1)))
      (global-stmt-exp (ID)
                       (let ([ref (expval->ref (apply-env ID the-global-env #t))])
                         (set! the-scope-env (extend-env ID (ref-val ref) the-scope-env))
                         (void-val)))
      (print-stmt-exp (atom)
                      (let ([val (value-of atom)])
                        (my-print val)
                        (displayln "")
                        (void-val)))
      (pass-exp ()
                (void-val))
      (break-exp ()
                 (break-val))
      (continue-exp ()
                    (continue-val))
      (function-def-exp (ID params p-body)
                        (let ([thunk-params
                               (map
                                (lambda (e)
                                  (list (car e) (a-thunk (cadr e) the-scope-env)))
                                (exp->params params))])
                          (let ([val (ref-val (newref (proc-val (a-proc ID thunk-params p-body))))])
                            (set! the-global-env (extend-env ID val the-global-env))
                            (set! the-scope-env (extend-env ID val the-scope-env))
                            (void-val))))
      (if-stmt-exp (exp1 exp2 exp3)
                   (let ([cnd (expval->bool (value-of exp1))])
                     (if cnd
                         (value-of exp2)
                         (value-of exp3))))
      (for-stmt-exp (ID lst statements)
                    (let ([lst (expval->list (value-of lst))])
                      (assign ID (none-exp))
                      (let ([iter-ref (expval->ref (apply-env ID the-scope-env #t))])
                        (let loop ([val-iter-list lst])
                          (if (null? val-iter-list)
                              (void-val)
                              (begin
                                (setref! iter-ref (car val-iter-list))
                                (let ([val (value-of statements)])
                                  (cases expval val
                                    (return-val (ret-val)
                                                val)
                                    (break-val ()
                                               (void-val))
                                    (continue-val ()
                                                  (loop (cdr val-iter-list)))
                                    (else
                                     (loop (cdr val-iter-list)))))))))))
      (params-exp (lst)
                  (report-must-not-reach-here))
      (or-exp (exp1 exp2)
              (let ([bool1 (expval->bool (value-of exp1))]
                    [bool2 (expval->bool (value-of exp2))])
                (bool-val (or bool1 bool2))))
      (and-exp (exp1 exp2)
               (let ([bool1 (expval->bool (value-of exp1))]
                     [bool2 (expval->bool (value-of exp2))])
                 (bool-val (and bool1 bool2))))
      (not-exp (exp1)
               (let ([bool1 (expval->bool (value-of exp1))])
                 (bool-val (not bool1))))
      (comparison-exp (sum compare-pairs)
                      (if (null? compare-pairs)
                          (bool-val #t)
                          (let ([left-val (value-of sum)]
                                [op (caar compare-pairs)]
                                [right-val (value-of (cadar compare-pairs))])
                            (cond
                              [(equal? op "==")
                               (cases expval left-val
                                 (num-val (num1)
                                          (let ([num2 (expval->num right-val)])
                                            (if (equal? num1 num2)
                                                (value-of (comparison-exp (num-exp num2) (cdr compare-pairs)))
                                                (bool-val #f))))
                                 (bool-val (bool1)
                                           (let ([bool2 (expval->bool right-val)])
                                             (if (equal? bool1 bool2)
                                                 (value-of (comparison-exp (bool-exp bool2) (cdr comparison-exp)))
                                                 (bool-val #f))))
                                 (else (report-type-error)))]
                              [(equal? op "<")
                               (let ([num1 (expval->num left-val)]
                                     [num2 (expval->num right-val)])
                                 (if (< num1 num2)
                                     (value-of (comparison-exp (num-exp num2) (cdr compare-pairs)))
                                     (bool-val #f)))]
                              [(equal? op ">")
                               (let ([num1 (expval->num left-val)]
                                     [num2 (expval->num right-val)])
                                 (if (> num1 num2)
                                     (value-of (comparison-exp (num-exp num2) (cdr compare-pairs)))
                                     (bool-val #f)))])
                            )))
      (add-exp (exp1 exp2)
               (let ([val1 (value-of exp1)])
                 (cases expval val1
                   (num-val (num1)
                            (let ([num2 (expval->num (value-of exp2))])
                              (num-val (+ num1 num2))))
                   (bool-val (bool1)
                             (let ([bool2 (expval->bool (value-of exp2))])
                               (bool-val (or bool1 bool2))))
                   (else (report-type-error)))))
      (sub-exp (exp1 exp2)
               (let ([num1 (expval->num (value-of exp1))]
                     [num2 (expval->num (value-of exp2))])
                 (num-val (- num1 num2))))
      (mul-exp (exp1 exp2)
               (let ([val1 (value-of exp1)])
                 (cases expval val1
                   (num-val (num1)
                            (if (zero? num1)
                                (num-val 0)
                                (let ([num2 (expval->num (value-of exp2))])
                                  (num-val (* num1 num2)))))
                   (bool-val (bool1)
                             (if (not bool1)
                                 (bool-val #f)
                                 (let ([bool2 (expval->bool (value-of exp2))])
                                   (bool-val (and bool1 bool2)))))
                   (else (report-type-error)))))
      (div-exp (exp1 exp2)
               (let ([num1 (expval->num (value-of exp1))]
                     [num2 (expval->num (value-of exp2))])
                 (num-val (/ num1 num2))))
      (factor-exp (sign exp1)
                  (let ([num1 (expval->num (value-of exp1))])
                    (if (equal? sign "+")
                        (num-val num1)
                        (num-val (- 0 num1)))))
      (power-exp (exp1 exp2)
                 (let ([num1 (expval->num (value-of exp1))]
                       [num2 (expval->num (value-of exp2))])
                   (num-val (expt num1 num2))))
      (call-exp (exp1 arguments)
                (let ([proc (expval->proc (value-of exp1))]
                      [old-scope-env the-scope-env]
                      [args (exp->arguments arguments)])
                  ;init scope-env for functino call
                  (set! the-scope-env (init-env #f))
                  ;add already defined functions to scope-env
                  (set! the-scope-env (extend-env-with-functions the-scope-env))
                  ;add arguments to scope-env
                  (cases procedure proc
                    (a-proc (ID params p-body)
                            (let loop ([args args]
                                       [params params])
                              (cond
                                [(and (null? params) (null? args)) 88]
                                [(null? params) (report-arguments-len-long)]
                                [(null? args)
                                 (let ([par-with-def (car params)])
                                   (set! the-scope-env (extend-env (car par-with-def) (ref-val (newref (cadr par-with-def))) the-scope-env))
                                   (loop args (cdr params)))]
                                [(let ([par-with-def (car params)])
                                   (set! the-scope-env (extend-env
                                                        (car par-with-def)
                                                        (ref-val (newref (a-thunk (car args) old-scope-env)))
                                                        the-scope-env))
                                   (loop (cdr args) (cdr params)))])))
                    (else
                     (report-type-error)))
                  ;run p-body and return value
                  (cases procedure proc
                    (a-proc (ID params p-body)
                            (let ([ret-val (value-of p-body)])
                              (set! the-scope-env old-scope-env)
                              (cases expval ret-val
                                (void-val () (none-val))
                                (return-val (val) val)
                                (else (report-type-error)))))
                    (else (report-type-error)))))
      (list-cell-exp (lst index)
                     (let ([l (expval->list (value-of lst))]
                           [num (expval->num (value-of index))])
                       (list-ref l num)))
      (arguments-exp (lst)
                     (report-must-not-reach-here))
      (var-exp (var-name)
               (let ([ref (expval->ref (apply-env var-name the-scope-env #t))])
                 (let ([w (deref ref)])
                   (if (expval? w)
                       w
                       (let ([val1 (value-of-thunk w)])
                         (setref! ref val1)
                         val1)))))
      (list-exp (exps-list)
                (list-val (map (lambda (e) (value-of e)) exps-list)))
      ;const exps
      (num-exp (num) (num-val num))
      (bool-exp (bool) (bool-val bool))
      (none-exp () (none-val))
      (list-const-exp (lst) (list-val lst))
      ;thunk exp
      (thunk-exp (th)
                 (cases thunk th
                   (a-thunk (exp env)
                            (value-of-thunk th))))
      )))

(define my-print
  (lambda (val)
    (cases expval val
      (num-val (num)
               (if (integer? num)
                   (print num)
                   (print (exact->inexact num))))
      (bool-val (bool)
                  (if bool
                      (display "True")
                      (display "False")))
      (none-val ()
                (display "None"))
        (list-val (lst)
                  (begin
                    (display "[")
                    (let loop ([lst lst])
                      (if (null? lst)
                          (void-val)
                          (begin
                            (my-print (car lst))
                            (if (null? (cdr lst))
                                (display "")
                                (display ", "))
                            (loop (cdr lst)))))
                    (display "]")))
      (else (report-type-error)))
    (void-val)))

(define assign
  (lambda (ID rhs)
    (if (global-scope? the-scope-env)
        (let ([res (apply-env ID the-scope-env #f)]
              [th (a-thunk (replace-var-exps rhs) the-scope-env)])
          (cases expval res
            (void-val ()
                      (let ([ref (newref th)])
                        (set! the-global-env (extend-env ID (ref-val ref) the-global-env))
                        (set! the-scope-env (extend-env ID (ref-val ref) the-scope-env))
                        (void-val)))
            (ref-val (ref)
                     (setref! ref th)
                     (void-val))
            (else (report-type-error))))
        (let ([res (apply-env ID the-scope-env #f)]
              [th (a-thunk (replace-var-exps rhs) the-scope-env)])
          (cases expval res
            (void-val ()
                      (let ([ref (newref th)])
                        (set! the-scope-env (extend-env ID (ref-val ref) the-scope-env))
                        (void-val)))
            (ref-val (ref)
                     (setref! ref th)
                     (void-val))
            (else (report-type-error)))))))

;------------------------------------------------------
;thunk
(define-datatype thunk thunk?
  (a-thunk
   (exp exp?)
   (saved-env env?)))

(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp saved-env)
               (let ([old-scope-env the-scope-env])
                 (set! the-scope-env saved-env)
                 (let ([val (value-of exp)])
                   (set! the-scope-env old-scope-env)
                   val))))))

(define replace-var-exps
  (lambda (e)
    (cases exp e
      (or-exp (exp1 exp2)
              (or-exp
               (replace-var-exps exp1)
               (replace-var-exps exp2)))
      (and-exp (exp1 exp2)
               (and-exp
                (replace-var-exps exp1)
                (replace-var-exps exp2)))
      (not-exp (exp)
               (not-exp (replace-var-exps exp)))
      (comparison-exp (sum compare-pairs)
                      (comparison-exp
                       (replace-var-exps sum)
                       (map (lambda (e) (replace-var-exps e)) compare-pairs)))
      (add-exp (exp1 exp2)
               (add-exp
                (replace-var-exps exp1)
                (replace-var-exps exp2)))
      (sub-exp (exp1 exp2)
               (sub-exp
                (replace-var-exps exp1)
                (replace-var-exps exp2)))
      (mul-exp (exp1 exp2)
               (mul-exp
                (replace-var-exps exp1)
                (replace-var-exps exp2)))
      (div-exp (exp1 exp2)
               (div-exp
                (replace-var-exps exp1)
                (replace-var-exps exp2)))
      (factor-exp (sign exp1)
                  (factor-exp sign (replace-var-exps exp1)))
      (power-exp (exp1 exp2)
                 (power-exp
                  (replace-var-exps exp1)
                  (replace-var-exps exp2)))
      (call-exp (proc arguments)
                (call-exp
                 (replace-var-exps proc)
                 (replace-var-exps arguments)))
      (arguments-exp (lst)
                     (arguments-exp (map (lambda (e) (replace-var-exps e)) lst)))
      (list-cell-exp (lst index)
                     (list-cell-exp
                      (replace-var-exps lst)
                      (replace-var-exps index)))
      (var-exp (var-name)
               (let ([w (deref (expval->ref (apply-env var-name the-scope-env #t)))])
                 (if (expval? w)
                     (cases expval w
                       (proc-val (proc) (var-exp var-name))
                       (num-val (num) (num-exp num))
                       (bool-val (bool) (bool-exp bool))
                       (none-val () (none-exp))
                       (list-val (lst) (list-const-exp lst))
                       (else (report-type-error)))
                     (thunk-exp w))))
      (list-exp (lst)
                (list-exp (map (lambda (e) (replace-var-exps e)) lst)))
      ;const exps
      (num-exp (num) e)
      (bool-exp (bool) e)
      (none-exp () e)
      (list-const-exp (lst) e)
      ;thunk exp
      (thunk-exp (th) e)
      (else (report-type-error)))))

;------------------------------------------------------
;expval
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst list?))
  (none-val)
  (proc-val (proc proc?))
  (void-val)
  (ref-val (ref number?))
  (break-val)
  (continue-val)
  (return-val(val expval?)))

;expval extractors
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-type-mismatch 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-type-mismatch 'bool val)))))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst) lst)
      (else (report-type-mismatch 'list val)))))

(define expval->ref
  (lambda (val)
    (cases expval val
      (ref-val (ref) ref)
      (else (report-type-mismatch 'ref val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-type-mismatch 'proc val)))))


;type error report functions
(define report-type-mismatch
  (lambda (expected val)
    (printf "Type mismatched: Expected ~s but got ~s\n" expected val)))

(define report-type-error
  (lambda ()
    (printf "Type error\n")))

;-------------------------------------------------------
;error report functions
(define report-must-not-reach-here
  (lambda ()
    (println "Must not reach here.")))

(define report-arguments-len-long
  (lambda ()
    (println "Arguments length is too long.")))

;-------------------------------------------------------
;proc
(define-datatype procedure proc?
  (a-proc
   (p-name string?)
   (params list?)
   (p-body exp?)))

;-------------------------------------------------------
;test: Tests' forlder is "tests"
(define test-dir "../tests/")
(define tests-list '(
                     "arithmetic_in.txt"
                     "call-by-need_in.txt"
                     "comparison_in.txt"
                     "default-param_in.txt"
                     "fib-function_in.txt"
                     "for-break_in.txt"
                     "for-continue_in.txt"
                     "for-simple_in.txt"
                     "global_in.txt"
                     "lhs-lazy-eval_in.txt"
                     "list_in.txt"
                     "list-max-func_in.txt"
                     "list-rec-function_in.txt"
                     "print_in.txt"
                     "sum-function_in.txt"
                     ))
(let loop ([tests-list tests-list])
  (if (null? tests-list)
      (void)
      (begin
        (evaluate (string-append test-dir (car tests-list)))
        (loop (cdr tests-list)))))
  







