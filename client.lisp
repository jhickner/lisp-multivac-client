; (proclaim '(optimize (speed 3) (space 0) (debug 0)))

; ccl -l "client.lisp" -e "(save \"ccl\" #'multivac-client:main)"
; sbcl --load "client.lisp" --eval "(save \"sbcl\" #'multivac-client:main)"

(require :drakma)
(require :cl-json)
(require :net-telent-date)
(require :getopt)

(defpackage :multivac-client
  (:use :cl :jwh :drakma :json :net.telent.date :cl-ppcre :getopt)
  (:export :main))
(in-package :multivac-client)


;"http://209.114.34.65:4567/"
(defvars *host* "http://209.114.34.65:4545/api/"
         *username* "653638dc733afce75130303fe6e6010f63768af0"
         *password* "X") 

(defvars c-on   (format nil "~C[36m" #\Esc) 
         c-on-b (format nil "~C[36;1m" #\Esc) 
         c-off  (format nil "~C[0m" #\Esc))

(push '("application" . "json") *text-content-types*)

;***********************
; HELPERS
;***********************

; 2011-08-07T17:18:27.682Z
(defun fix-date (d)
  (universal-time-to-rfc2822-date 
    (parse-time 
      (values (regex-replace-all "T|\\.\\d{3}Z" d " "))) 
    0))

(defun parse-tags (tags)
  (if (stringp tags)
    (mapcar #'string-downcase (split "\\s+|\\," tags))
    tags))

(defun output-item (item)
  (progn
    (format t "~a[ ~{~a~^, ~} ]-[ ~a ]-[ ~a ]~a~%" 
            c-on
            (aget :tags item)
            (aget :$oid (aget :--id item))
            (fix-date (aget :$date (aget :ts item)))
            c-off)
    (when-let body (aget :body item)
              (format t "~a~%" body))
    (when-let link (aget :link item)
              (format t "~alink:~a ~a~%" c-on c-off link))
    (format t "~%")))

(defun output-tag-count (items)
  (let ((tags (decode-json-from-string items)))
    (dolist (tag tags)
      (format t "~a~a~a: ~a~%"
              c-on
              (aget :--id tag)
              c-off
              (values (round (aget :value tag)))))))

;***********************
; SERVER OPS
;***********************

(defmacro server-request (path &rest params)
  `(values 
     (funcall #'http-request 
            (str ,*host* ,path)
            :basic-authorization '(,*username* ,*password*) 
            ,@params)))

(defun search-items (tags)
  (if tags
    (let ((body 
             (decode-json-from-string
               (server-request (str "search/" (join "," tags))))))
      (dolist (b body) (output-item b)))))

(defun delete-item (id)
  (format t "deleting: ~a~%" id)
  (server-request (str "item/" id) :method :delete))

(defun add-item (tags body &optional link)
  (let* ((payload `((:tags . ,tags)
                    (:body . ,body)))
         (payload (if link (cons `(:link . ,link) payload)
                    payload)))
    (server-request "item"
                    :method :post
                    :additional-headers '(("Content-Type" . "application/json"))
                    :content (encode-json-to-string payload))))

(defun get-tag-count ()
  (output-tag-count (server-request "tag-count")))

;***********************
; ARG PROCESSING
;***********************

(defun handle-args (args opts)
  (let ((cmd (car args))
        (del-id (aget "delete" opts :test #'string=))
        (link (aget "link" opts :test #'string=)))
    (cond 
      ((equal cmd "add")
       (format t "result: ~S~%" 
              (add-item (parse-tags (cadr args)) 
                        (join " " (cddr args)) 
                        link)))
      ((equal cmd "tags")
       (get-tag-count))
      (T (if del-id
           (format t "result: ~S~%" (delete-item del-id))
           (search-items args))))))


;***********************
; MAIN
;***********************
(defun main ()
  (let ((args (or
                #+ccl (subseq ccl:*command-line-argument-list* 2)
                #+sbcl (rest sb-ext:*posix-argv*)
      nil)))
    (multiple-value-bind (a b)
      (getopt args '(("delete" :optional)
                     ("link" :optional)))
      (handle-args a b))))
