(defpackage :jwh
  (:export #:defvars
           #:defparams
           #:get-args
           #:quit
           #:catch-errors
           #:file-string
           #:aget
           #:flatten
           #:partition
           #:str
           #:join
           #:parse-int
           #:when-let
           #:if-let
           )
  (:use :cl))

(in-package :jwh)

;**********************
; DOCUMENTATION, ETC.
;**********************

(defmacro :? (thing)
  `(describe ',thing))

(defmacro :doc (f &optional (type 'function)) 
  `(documentation ',f ',type)) 

;**********************
; MISC
;**********************

(defmacro defvars (&rest args)
  (cons 'progn 
        (loop while args
              collect (list 'defvar (pop args) (pop args)))))

(defmacro defparams (&rest args)
  (cons 'progn 
        (loop while args
              collect (list 'defparameter (pop args) (pop args)))))

(defun get-args ()
  (or
    #+ccl (subseq ccl:*command-line-argument-list* 2)
    #+sbcl (rest sb-ext:*posix-argv*)
    nil))

(defun quit () 
  #+:ccl (ccl:quit)
  #+:sbcl (sb-ext:quit))

(defmacro catch-errors (&body body)
  `(handler-case
     (progn
       ,@body)
     (error (e) (format t "Error: ~S~%" e))))

;**********************
; FILES
;**********************

(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (read-sequence data s)
      data)))


;**********************
; ASSOCIATION LISTS
;**********************

(defun aget (item a-list &rest keys)
  (cdr (apply #'assoc item a-list keys)))


;**********************
; LISTS
;**********************

(defun flatten (lis)
  (cond ((atom lis) lis)
        ((listp (car lis))
         (append (flatten (car lis)) (flatten (cdr lis))))
        (t (append (list (car lis)) (flatten (cdr lis))))))

(defun partition (list length)
  (loop
     while list
     collect (subseq list 0 length)
     do (setf list (nthcdr length list))))


;**********************
; STRINGS
;**********************

(defun str (&rest args) 
  (apply #'concatenate (cons 'string args)))

(defun join (delim strings)
  (reduce (lambda (a b)
            (concatenate 'string a delim b))
          strings))


;**********************
; NUMBERS
;**********************

; saner defaults
(defun parse-int (s)
  (parse-integer s :junk-allowed t))

;**********************
; CONDITIONALS
;**********************

; accepts only 1 binding
(defmacro when-let (b expr &rest body)
  (let ((form b) (tst expr) (g (gensym)))
    `(let ((,g ,tst))
       (when ,g
         (let ((,form ,g))
           ,@body)))))

(defmacro if-let (b expr exprt exprf)
  (let ((form b) (tst expr) (g (gensym)))
    `(let ((,g ,tst))
       (if ,g
         (let ((,form ,g))
           ,exprt)
         ,exprf))))

