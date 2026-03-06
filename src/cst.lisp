(uiop:define-package #:40ants-lisp-dev-mcp/cst
  (:use #:cl)
  (:import-from #:eclector.parse-result
                #:parse-result-client
                #:make-expression-result
                #:make-skipped-input-result)
  (:import-from #:eclector.base)
  (:import-from #:eclector.reader)
  (:export #:cst-node
           #:make-cst-node
           #:cst-node-kind
           #:cst-node-value
           #:cst-node-children
           #:cst-node-start
           #:cst-node-end
           #:cst-node-start-line
           #:cst-node-end-line
           #:parse-top-level-forms))
(in-package #:40ants-lisp-dev-mcp/cst)


(defstruct cst-node
  "A node in a concrete syntax tree with source positions."
  (kind :expr :type keyword)
  (value nil)
  (children nil :type list)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (start-line 1 :type fixnum)
  (end-line 1 :type fixnum))


;;; Eclector parse-result client that tracks character offsets

(defclass cst-client (eclector.parse-result:parse-result-client)
  ((source-text :initarg :source-text :reader client-source-text)))

(defmethod eclector.base:source-position ((client cst-client) stream)
  (file-position stream))

(defmethod eclector.base:make-source-range ((client cst-client) start end)
  (cons start end))

(defmethod eclector.reader:interpret-symbol
    ((client cst-client) input-stream
     package-indicator symbol-name internp)
  "Intern symbols leniently: create missing packages on the fly so that
   parsing arbitrary Lisp source never fails due to unknown packages."
  (let ((package
          (cond
            ((null package-indicator)
             (return-from eclector.reader:interpret-symbol
               (make-symbol symbol-name)))
            ((eq package-indicator :keyword) (find-package :keyword))
            ((eq package-indicator :current) *package*)
            (t (or (find-package package-indicator)
                   (make-package package-indicator :use '()))))))
    (if internp
        (intern symbol-name package)
        (let ((sym (find-symbol symbol-name package)))
          (or sym (intern symbol-name package))))))

(defmethod eclector.reader:evaluate-expression ((client cst-client) expression)
  "Suppress read-time evaluation for safety."
  expression)

(defun count-newlines-before (text position)
  "Count line number (1-based) at POSITION in TEXT."
  (1+ (count #\Newline text :end (min position (length text)))))

(defmethod eclector.parse-result:make-expression-result
    ((client cst-client) result children source)
  (let ((start (car source))
        (end (cdr source))
        (text (client-source-text client)))
    (make-cst-node :kind :expr
                   :value result
                   :children (remove nil children)
                   :start start
                   :end end
                   :start-line (count-newlines-before text start)
                   :end-line (count-newlines-before text end))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client cst-client) stream reason children source)
  (declare (ignore stream children))
  (when (member reason '(:line-comment :block-comment
                         (:sharpsign-plus :if) (:sharpsign-plus :else)
                         (:sharpsign-minus :if) (:sharpsign-minus :else))
                :test #'equal)
    (let ((start (car source))
          (end (cdr source))
          (text (client-source-text client)))
      (make-cst-node :kind :skipped
                     :value reason
                     :start start
                     :end end
                     :start-line (count-newlines-before text start)
                     :end-line (count-newlines-before text end)))))


(defun parse-top-level-forms (text)
  "Parse TEXT into a list of CST-NODE objects representing top-level forms.
   Returns all forms including comments."
  (let* ((client (make-instance 'cst-client :source-text text))
         (results '())
         (*package* (find-package :cl-user))
         (*read-eval* nil))
    (with-input-from-string (stream text)
      (loop
        (multiple-value-bind (result orphans)
            (eclector.parse-result:read client stream nil stream)
          (when (eq result stream)
            ;; Collect any trailing orphans (comments after last form)
            (when orphans
              (dolist (o orphans)
                (when o (push o results))))
            (return))
          ;; Collect orphans that precede this form (e.g. comments)
          (when orphans
            (dolist (o orphans)
              (when o (push o results))))
          (push result results))))
    (nreverse results)))
