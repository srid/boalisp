(require (lib "match.ss"))
(require (only (lib "1.ss" "srfi") fold))
(require (only (lib "13.ss" "srfi") string-contains))
(require "python.ss")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Construction
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Match function arguments
;; (def foo (a b c (d 10) (e "foo") (* args) (* kwargs)) ...)
(define (match-function-args args)
  ;; I wish this was a match-lambda ...
  (define (valid-arg? arg)
    (and (symbol? arg)
         (not (eq? arg '*))))
  (let ((*args*     (list))
        (*defaults* (list))
        (*flag*     0))
    (let loop ()
      (when (and (not (null? args)) (valid-arg? (car args)))
        (set! *args* (append *args* (list (car args))))
        (set! args (cdr args))
        (loop)))
    ((match-lambda
       (((vars vals) ...)
        (set! *defaults* vals)
        (set! *args* (append *args* vars)))) args)
    (values *args* *defaults* *flag*)))

(define (not-symbol sym)
  (lambda (s)
    (not
     (and (symbol? s)
          (string=? (symbol->string s)
                    sym)))))

(define T
  (match-lambda
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; MATH operators
    
    (('+ left right)
     (@> Add (list (T left) (T right))))
    (('+ args ...)
     ;; fold into multiple forms of (+ A B)
     ;; eg: (+ a b c d) => (+ (+ (+ a b) c) d)
     (T (fold (lambda (a b) `(+ ,b ,a))
              (car args)
              (cdr args))))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; BOOLEAN operators
    
    (('and nodes ...)
     (@> And : (map T nodes)))
    (('not node)
     (@> Not (T node)))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; COMPARISON operators
    
    (('= first rest ...)
     (@> Compare 
         (T first)
         (map (lambda (r)
                (py/tuple (list (ss->py "==") (T r))))
              rest)))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Variable ASSIGNMENT
    
    (('set! (targets ...) value)
     (@> Assign
         (list (@> AssTuple
                   (map (lambda (t)
                          (unpack-assign-name t))
                        targets)))
         (T value)))
    
    (('set! target value)
     (@> Assign 
         (list (unpack-assign-name target))
         (T value)))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; TUPLE, LIST, DICT
    
    ((':tuple items ...)
     (@> Tuple (map T items)))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; IF, COND
    
    ;;;; (if <exp> (begin ...) else (begin ...)
    (('if exp ('begin body1 ...) ('begin body2 ...))
     (@> If
         (list (py/tuple (list (T exp) (@> Stmt (map T body1)))))
         (@> Stmt (map T body2))))
    
    (('if exp ('begin body1 ...))
     (@> If
         (list (py/tuple (list (T exp) (@> Stmt (map T body1)))))
         py/none))
    
    (('if exp body1 body2)
     (T `(if ,exp (begin ,body1) (begin ,body2))))
    
    (('if exp body1)
     (@> If
         (list (py/tuple (list (T exp) (@> Stmt (list (T body1))))))
         py/none))
    
    (('cond ((and exp (? (not-symbol "else"))) body ...) ...)
     (@> If
         (map (lambda (e b)
                (py/tuple (list (T e) (@> Stmt (map T b)))))
              exp body)
         py/none))
    ; TODO: else
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; LOOP constructs
    
    (('for var 'in seq suite ...)
     (@> For
         (@> AssName var "OP_ASSIGN")
         (T seq)
         (@> Stmt (map T suite))
         py/none))
    
    (('while cond body ...)
     (@> While
         (T cond)
         (@> Stmt (map T body))
         py/none))                    ;; TODO: else
    
    (('break)
     (@> Break))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; FUNCTION definition
    
    (('def name (args ...) body ...)
     (display args)(newline)
     (call-with-values 
      (lambda () (match-function-args args))
      (lambda (args defaults flag)
        (let ((ast 
               (@> Function
                   py/none
                   name
                   (map ss->py args) ;; XXX: @> macro does not do
		                     ;; a deep translation of types
                   (map T defaults)          
                   flag              ;; TODO: flags
                   py/none           ;; TODO: doc string
                   (@> Stmt (map T body)))))
          (filename-hack! ast)
          ast))))
    
    (('return exp)
     (@> Return (T exp)))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; CLASS definition
    
    (('class name (bases ...) statements ...)
     (let ((class (@> Class
		      name bases "DOCSTRING"
		      (@> Stmt (map T statements)))))
       (filename-hack! class)
       class))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PRINT, IMPORT, ....
    
    ;;;; (print x)
    (('print exps ...) ;; ???
     (@> Printnl (map T exps) py/none))
    
    ;; >>> print e1, e2,
    (('display exps ...)
     (@> Print (map T exps) py/none))
    
    ;;;; (import name1 name2 ...)
    (('import names ...)
     (@> Import (map (lambda (n)
                       (py/tuple (list (ss->py n) py/none)))
                     names)))
    
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; APPLY
    
    ((proc args ...)
     (@> CallFunc (T proc) (map T args) 
         py/none py/none))               ;; TODO: *args, **kwargs
   
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; CONSTANTS, VARIABLES
    
    ;;;; a token -- var, 23, "foobar", 634.34
    (name
     (cond 
       ((symbol? name) (unpack-name name))
       ((number? name) (@> Const name))
       ((string? name) (@> Const name))
       (else (error "invalid exp" name))))))

;; These functions handle names of the forms,
;;  - foo.bar.baz
;;  - foo
;; generating appropriate AST nodes.
;;
;; XXX: In future, this functionality must become part of the Lisp ``reader''
;;
(define (unpack-name name)
  (let ((name-s (symbol->string name)))
    (if (dotted-name? name-s)
        (let ((names (str-split name-s #\.)))
          (fold (lambda (a b) (@> Getattr b a))
                (T (string->symbol (car names)))
                (cdr names)))
        (@> Name name))))

(define (unpack-assign-name name)
  (let ((name-s (symbol->string name)))
    (if (dotted-name? name-s)
        (let* ((names* (str-split name-s #\.))
               (names  (all-but-last names*))
               (lname  (last names*)))
          (@> AssAttr 
              (fold (lambda (a b) (@> Getattr b a))
                            (T (string->symbol (car names)))
                            (cdr names))
              lname
              "OP_ASSIGN"))
        (@> AssName name "OP_ASSIGN"))))

(define (all-but-last l)
  (cond ((null? (cdr l)) null)
        (else (cons (car l) (all-but-last (cdr l))))))

(define (last l)
  (cond ((null? (cdr l)) (car l))
        (else (last (cdr l)))))

(define (dotted-name? name)
  (cond
    ((symbol? name)
     (string-contains (symbol->string name) "."))
    ((string? name)
     (string-contains name "."))
    (else
     (error "wrong type for `name' --" name))))

;; From http://schemecookbook.org/Cookbook/StringSplit
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
        ((split
          (lambda (a b)
            (cond
              ((>= b len)
	       (if (= a b)
		   '("")
		   (cons (substring str a b) '())))
              ((char=? ch (string-ref str b))
	       (if (= a b)
		   (cons "" (split (+ 1 a) (+ 1 b)))
		   (cons (substring str a b)
			 (split (+ 1 b) (+ 1 b)))))
              (else (split a (+ 1 b)))))))
      (split 0 0))))   


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Code Generation
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-code-obj co filename filename-pyc)
  (let ((f (@ (: "__builtin__" 'open) filename-pyc "wb")))
    (@ (: f 'seek) 0 0)
    (@ (: f 'write) (@ (: "imp" 'get_magic)))
    (let ((ts (py/int-as-long
               (@ (: "__builtin__" 'long)
                  (py/getattr (@ (: "os" 'stat)
                                 filename)
                              "st_mtime")))))
      (map (lambda (n)
             (@ (: f 'write)
                (@ (: "__builtin__" 'chr) n)))
           (map (lambda (n)
                  (bitwise-and (arithmetic-shift ts (- n)) #xff))
                (list 0 8 16 24))))
    (@ (: "marshal" 'dump) co f)
    (@ (: f 'flush))
    (@ (: f 'close))))

(define (read-sexp fileh)
  (let ((sexp (read fileh)))
    (if (eof-object? sexp)
        (list)
        (cons sexp (read-sexp fileh)))))

;; XXX: TODO:
;; I do not understand why I have to do this.
;; Modules, Functions, Classes "must" have the `filename' attribute?
(define *filename-hack* null)
(define (filename-hack! obj)
  (py/setattr obj "filename" (py/str *filename-hack*)))

(define (compile-boa filename filename-pyc)
  (set! *filename-hack* filename)
  (call-with-input-file filename
    (lambda (i)
      (let* ((ast (@> Module "<DOCSTRING>" (@> Stmt (map T (read-sexp i)))))
             (_   (filename-hack! ast))
             (c   (@ (: "compiler" 'pycodegen 'InteractiveCodeGenerator) ast))
             (co  (@ (: c 'getCode))))
        ;; (py/err-print)
        (write-code-obj co filename filename-pyc)
        ;; diassemble it for debugging
        ;; (@ (: "dis" 'dis) co)
        'ok))))

;; Primitive command line options interface.
(define cmd-arg
  (let ((ARGS '((source 0 "examples/test.boa")
                (output 1 "test.pyc"))))
    (lambda (arg)
      (let loop ((a ARGS))
        (if (eq? arg (caar a))
            (if (< (cadar a) (vector-length argv))
                (vector-ref argv (cadar a))
                (caddar a))
            (loop (cdr a)))))))

(compile-boa (cmd-arg 'source) (cmd-arg 'output))
