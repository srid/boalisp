(module python mzscheme
  (require (only (lib "1.ss" "srfi") fold)
           (lib "foreign.ss")
           (lib "kw.ss"))
  
  (provide (all-defined))
 
  
  (unsafe!)
  
  (define libpy (ffi-lib "python25"))
  
  (define py-initialize
    (get-ffi-obj "Py_Initialize" libpy (_fun -> _void)))
  
  (py-initialize)  ;; It is the responsibility of the module importer 
                   ;; to call (py-finalize)
  
  (define py-finalize
    (get-ffi-obj "Py_Finalize" libpy (_fun -> _void)))
  
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Basic Python objecs
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define py/obj
    (make-ctype _pointer #f #f))
  
  (define py/str
    (get-ffi-obj "PyString_FromString" libpy 
                 (_fun _string -> py/obj)))
  
  (define py/int
    (get-ffi-obj "PyInt_FromLong" libpy
                 (_fun _long -> py/obj)))
  
  (define py/str->string
    (get-ffi-obj "PyString_AsString" libpy
                 (_fun py/obj -> _string)))
  
  (define py-run-simple-string
    (get-ffi-obj "PyRun_SimpleString" libpy
                 (_fun _string -> _void)))
  
  (define py/apply
    (get-ffi-obj "PyObject_CallObject" libpy
                 (_fun py/obj py/obj -> py/obj)))
  
  (define py/import
    (get-ffi-obj "PyImport_Import" libpy
                 (_fun py/obj -> py/obj)))
  
  (define py/import-module
    (get-ffi-obj "PyImport_ImportModule" libpy
                 (_fun _string -> py/obj)))
  
  (define py/getattr
    (get-ffi-obj "PyObject_GetAttrString" libpy
                 (_fun py/obj _string -> py/obj)))
  
  (define py/setattr
    (get-ffi-obj "PyObject_SetAttrString" libpy
                 (_fun py/obj _string py/obj ->  _int)))
  
  (define py/getitem
    (get-ffi-obj "PySequence_GetItem" libpy
                 (_fun py/obj _int -> py/obj)))
  
  (define py/err-occured
    (get-ffi-obj "PyErr_Occurred" libpy
                 (_fun -> _pointer)))
  
  (define py/err-print
    (get-ffi-obj "PyErr_Print" libpy
                 (_fun -> _void)))
  
 
  ;; Convert `args*' to a Python tuple
  (define (py/tuple args*)
    (let ((py/tuple-new (get-ffi-obj "PyTuple_New" libpy
                                     (_fun _int -> py/obj)))
          (py/tuple-set (get-ffi-obj "PyTuple_SetItem" libpy
                                     (_fun py/obj _int py/obj -> _void))))
      (let ((t (py/tuple-new (length args*))))
        (define (set-in-tuple args i)
          (if (not (null? args))
              (begin
                ;(display (format "-- ~a\n" (car args)))
                (py/tuple-set t i (car args))
                (set-in-tuple (cdr args) (+ i 1)))))
        ;(display (format "start - ~a\n" (length args*)))
        (set-in-tuple args* 0)
        ;(display "-end\n")
        t)))
  
  ;; Convert `args' to a Python list
  (define (py/list args)
    (let ((py/list-new    (get-ffi-obj "PyList_New" libpy
                                       (_fun _int -> py/obj)))
          (py/list-append (get-ffi-obj "PyList_Append" libpy
                                       (_fun py/obj py/obj -> _int))))
      (let ((l (py/list-new 0)))
        (define (append-car args)
          (when (not (null? args))
            (py/list-append l (car args))
            (append-car (cdr args))))
        (append-car args)
        l)))
  
  
  (define py/int-as-long
    (get-ffi-obj "PyInt_AsLong" libpy
                 (_fun py/obj -> _long)))
  
  ;; The Python ``None'' object.
  ;; Note: _Py_NoneStruct is the ``unofficial'' and ``internal'', that is
  ;;       accessible via a C macro.
  (define py/none 
    (ffi-obj-ref "_Py_NoneStruct" libpy ))
  
  ;; Attribute access for Pythobn objects.
  ;; If the first argument is a ``string'', it is imported as module.
  ;; eg: (py/dot "os" 'path 'join)
  (define py/dot
    (lambda/kw (#:rest names)
      (define (attr-ref a b)
        ;; b.a
        (if (null? b)
            (if (string? a)
                (py/mod a)
                a)
            (py/getattr b (symbol->string a))))
      (fold attr-ref null names)))
  
  ;; Import the Python module ``modname''
  (define (py/mod modname)
    (py/import (py/str modname)))
  
  ;; Use Python's ``apply'' to call the given function
  ;; eg: (py/call (py/dot "os" 'path 'join) (py/str "/usr") (py/str "share"))
  (define py/call
    (lambda/kw (func #:rest args)
      (py/apply func (py/tuple args))))
  
  ;; Syntax sugar for py/call
  ;; Usage:
  ;;   (@ (: "os" 'path 'join) "/usr" "share"))
  (define-syntax @
    (syntax-rules (:)
      ((_ (: flist ...) arg ...)
       (@ (py/dot flist ...) arg ...))
      ((_ f arg ...)
       (py/apply f (py/tuple (map ss->py (list arg ...)))))))
  
  ;; Convert a Scheme type to corresponding Python type.
  ;; As fail case, return the Scheme type as such.
  (define (ss->py o)
    (cond
      ((null? o)   (py/list o))
      ((pair? o)   (py/list o))
      ((symbol? o) (py/str (symbol->string o)))
      ((string? o) (py/str o))
      ((number? o) (py/int o))
      (else o)))
  
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ``compiler.ast'' module
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define ast
    (let ((ast (py/import-module "compiler.ast")))
      (lambda (node-type)
        (lambda/kw (#:rest r)
          (py/apply (py/getattr ast node-type)
                    (py/tuple r))))))
  
  ;; Syntax sugar for constructing AST nodes
  ;; Usage:
  ;;   (@> Add (@> Name "a") (@> Name "b"))
  ;;   (@> And : (nodes ...))
  (define-syntax @>
    (syntax-rules (:)
      ((_ node : c-list)
       (apply (ast (symbol->string (car `(node))))
              (map ss->py c-list)))
      ((_ node c1 ...)
       (@> node : (list c1 ...)))))
  
  'ok)