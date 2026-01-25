(uiop:define-package #:40ants-lisp-dev-mcp/core
  (:use #:cl)
  (:import-from #:40ants-mcp)
  (:import-from #:40ants-logging)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:openrpc-server)
  (:import-from #:jsonrpc/errors)
  (:import-from #:log :info)
  (:import-from #:40ants-slynk
                #:start-slynk-if-needed)
  (:import-from #:alexandria)
  (:import-from #:40ants-mcp/content/text
                #:text-content)
  (:import-from #:40ants-mcp/server/errors
                #:tool-error)
  (:import-from #:40ants-mcp/tools
                #:define-tool)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:export #:start-server))
(in-package #:40ants-lisp-dev-mcp/core)


(openrpc-server:define-api (dev-tools :title "Lisp dev tools"))


(define-tool (dev-tools eval-lisp-form) (form &key (in-package "CL-USER"))
  (:summary "Evaluates a given Lisp form and returns a list of values.

             Only one lisp form should be provided as the input.
             If you need to eval a multiple forms, wrap them into
             a PROGN or a similar form.

             A multiple values can be returned. Each value is printed in it's own
             section with a title like VALUE-1, VALUE-2 and so on.

             Also this tool returns STDOUT and STDERR if something was written to these streams.

             In case of an error, the ERROR result with a backtrace will be returned.

             If you need to evaluate form in context of some package other than CL-USER,
             then pass package name in IN-PACKAGE argument.
             All FORM symbols without package qualifier, will be interned into this package.")
  (:param form string "Lisp form to be evaluated, in the s-expression syntax.")
  (:param in-package string "Common Lisp package name to evaluate form in.")
  (:result (soft-list-of text-content))

  (block func
    (with-output-to-string (stdout-stream)
      (with-output-to-string (stderr-stream)
        (let ((*standard-output* stdout-stream)
              (*error-output* stderr-stream))
          (flet ((make-output-results ()
                   (let ((stdout (str:trim (get-output-stream-string stdout-stream)))
                         (stderr (str:trim (get-output-stream-string stderr-stream))))
                     (append (unless (str:emptyp stdout)
                               (list (make-instance 'text-content
                                                    :text (fmt "## STDOUT~2%~A"
                                                               stdout))))
                             (unless (str:emptyp stderr)
                               (list (make-instance 'text-content
                                                    :text (fmt "## STDERR~2%~A"
                                                               stderr))))))))
            (let* ((result-values
                     (multiple-value-list
                      (handler-bind ((serious-condition
                                       (lambda (c)
                                         (let ((error-message
                                                 (with-output-to-string (s)
                                                   (format s "## ERROR~2%")
                                                   (trivial-backtrace:print-condition c s))))
                                           (error 'tool-error
                                                  :content (list* (make-instance 'text-content
                                                                                 :text error-message)
                                                                  (make-output-results)))))))
                        (let* ((*package* (or (find-package in-package)
                                              (find-package (string-upcase in-package))
                                              (error 'tool-error
                                                     :content (list* (make-instance 'text-content
                                                                                    :text (fmt "Package \"~A\" was not found."
                                                                                               in-package))))))
                               (package-name (package-name *package*))
                               (forms (uiop:with-safe-io-syntax (:package package-name)
                                        (with-input-from-string (s form)
                                          (uiop:slurp-stream-forms s))))
                               ;; To allow eval multiple forms, we need to wrap
                               ;; them with PROGN:
                               (expression
                                 (list* 'progn
                                        forms)))
                          (eval expression))))))

              (return-from func
                (append
                 (loop for value in result-values
                       for idx upfrom 1
                       collect (make-instance 'text-content
                                              :text (fmt "## VALUE-~A~2%~A"
                                                         idx
                                                         value)))
                 (make-output-results))))))))))


(defun start-server (&key port debug (in-thread nil))
  "Starts the MCP server.

   PORT: TCP port number (integer or nil for stdio transport).
   DEBUG: Boolean to enable debugger on errors.
   IN-THREAD: Boolean, if true starts server in a background thread, otherwise blocks (default).

   Returns thread object if IN-THREAD is true, otherwise blocks."
  (let ((server-fn (lambda ()
                     (40ants-mcp/server/definition:start-server dev-tools
                                                                :transport (if port
                                                                             :http
                                                                             :stdio)
                                                                :port port))))
    (if in-thread
        (make-thread server-fn :name "MCP Server Thread")
        (funcall server-fn))))
