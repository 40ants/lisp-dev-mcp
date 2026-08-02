(uiop:define-package #:40ants-lisp-dev-mcp/core
  (:use #:cl)
  (:import-from #:40ants-mcp)
  (:import-from #:40ants-logging)
  (:import-from #:serapeum
                #:fmt
                #:href
                #:dict
                #:->)
  (:import-from #:openrpc-server)
  (:import-from #:jsonrpc/errors)
  (:import-from #:log :info)
  (:import-from #:40ants-slynk
                #:start-slynk-if-needed)
  (:import-from #:alexandria
                #:write-string-into-file)
  (:import-from #:40ants-mcp/content/text
                #:text-content)
  (:import-from #:40ants-mcp/server/errors
                #:tool-error)
  (:import-from #:40ants-mcp/server/definition)
  (:import-from #:40ants-mcp/tools
                #:define-tool)
  (:import-from #:bordeaux-threads-2
                #:make-thread)
  (:import-from #:yason)
  (:import-from #:str
                #:split)
  (:import-from #:find-port
                #:find-port
                #:port-open-p)
  (:export #:start-server
           #:*opencode-config-pathname*
           #:choose-port
           #:get-port-from-assistant-config
           #:update-port-in-config
           #:read-config
           #:write-config
           #:make-default-config))
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


(defvar *opencode-config-pathname*
  #P"opencode.json"
  "Pathname of the Opencode config file which is updated when
START-SERVER is called with :UPDATE-CONFIG T.

You can rebind this special variable or pass an explicit
:OPENCODE-CONFIG argument to START-SERVER, CHOOSE-PORT,
GET-PORT-FROM-ASSISTANT-CONFIG and UPDATE-PORT-IN-CONFIG.")


(-> read-config (pathname)
    (values hash-table &optional))

(defun read-config (path)
  (yason:parse path
               :json-arrays-as-vectors nil
               :json-booleans-as-symbols t
               :json-nulls-as-keyword t))


(-> write-config (pathname hash-table)
    (values &optional))

(defun write-config (file data)
  (let ((content (yason:with-output-to-string* (:indent 4)
                   (yason:encode data))))
    (write-string-into-file content
                            file
                            :if-exists :supersede)
    (values)))


(-> make-default-config ()
    (values hash-table &optional))

(defun make-default-config ()
  (dict "$schema" "https://opencode.ai/config.json"
        "skills" (dict "paths"
                       #(".agents/skills"))
        "mcp" (dict "lisp-dev-mcp"
                    (dict "type" "remote"
                          "url" "to be replaced"))))


(-> get-port-from-assistant-config (&key (:config pathname))
    (values (or null integer) &optional))

(defun get-port-from-assistant-config (&key (config *opencode-config-pathname*))
  "Returns the port recorded in the Opencode config, or NIL."
  (let ((file (probe-file config)))
    (when file
      (let* ((data (read-config file))
             (url (href data "mcp" "lisp-dev-mcp" "url")))
        (when url
          (let ((third-part (third (split #\: url))))
            (when third-part
              (let ((port-as-str (first (split #\/ third-part))))
                (values (parse-integer port-as-str))))))))))


(-> update-port-in-config (integer &key (:config pathname))
    (values &optional))

(defun update-port-in-config (port &key (config *opencode-config-pathname*))
  "Writes the given PORT into the Opencode config file,
creating a default config when the file does not exist yet."
  (let* ((file (probe-file config))
         (data (if file
                 (read-config file)
                 (make-default-config)))
         (url (fmt "http://localhost:~A/mcp"
                   port)))
    (setf (href data "mcp" "lisp-dev-mcp" "url")
          url)
    (write-config config data)
    (values)))


(-> choose-port ((or integer (eql :auto) string) &key (:config pathname))
    (values integer boolean &optional))

(defun choose-port (port &key (config *opencode-config-pathname*))
  "Resolves PORT into a concrete TCP port number and returns it as the first value.

As the second value returns T when the resolved port differs from the one
recorded in the Opencode config.

   PORT can be:
     - an INTEGER, used as-is after checking it is free;
     - the :AUTO keyword (or the string \"auto\"), in which case a free port
       is selected automatically, reusing the port from the Opencode config
       when it is still available."
  (let* ((port-from-assistant-config
           (get-port-from-assistant-config :config config))
         (port-to-return
           (cond
             ((or (eql port :auto)
                  (and (stringp port)
                       (string-equal port "auto")))
              (cond
                ;; Try to reuse port from Opencode's config
                ;; if MCP is already configured
                ((and port-from-assistant-config
                      (port-open-p port-from-assistant-config))
                 port-from-assistant-config)
                ;; Otherwise we will choose a new port
                (t
                 (find-port))))
             (t
              (let ((parsed (etypecase port
                              (integer port)
                              (string (parse-integer port)))))
                (unless (port-open-p parsed)
                  (error "Port ~A already taken by other program."
                         parsed))
                parsed)))))
    (values port-to-return
            (not
             (equal port-to-return
                    port-from-assistant-config)))))


(defun start-server (&key port (in-thread t) update-config (opencode-config *opencode-config-pathname*))
  "Starts the MCP server.

   PORT controls the transport and the port:
     - NIL (the default) uses the stdio transport;
     - an INTEGER uses the Streaming HTTP transport on that TCP port;
     - :AUTO selects a free TCP port automatically, reusing the port from
       the Opencode config when it is still available.

   IN-THREAD controls whether the server runs in a background thread (the
   default) or blocks the caller.

   When UPDATE-CONFIG is true and a port was selected (or reused), the chosen
   port is written into the Opencode config file pointed to by OPENCODE-CONFIG
   \(which defaults to *OPENCODE-CONFIG-PATHNAME*).

   Returns the server thread when IN-THREAD is true, otherwise blocks."
  (multiple-value-bind (chosen-port port-differs-from-config)
      (when port
        (choose-port port :config opencode-config))
    
    (when (and chosen-port
               update-config
               port-differs-from-config)
      (update-port-in-config chosen-port :config opencode-config))
    
    (flet ((server-fn ()
             (40ants-mcp/server/definition:start-server dev-tools
                                                        :transport (if chosen-port
                                                                     :http
                                                                     :stdio)
                                                        :port chosen-port)))
      (if in-thread
        (make-thread #'server-fn :name "MCP Server Thread")
        (funcall #'server-fn)))))
