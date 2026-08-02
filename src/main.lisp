(uiop:define-package #:40ants-lisp-dev-mcp/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:40ants-lisp-dev-mcp/core
                #:start-server)
  (:import-from #:40ants-logging)
  (:import-from #:40ants-slynk
                #:start-slynk-if-needed)
  (:import-from #:jsonrpc/errors)
  (:import-from #:log)
  (:import-from #:yason)
  (:import-from #:serapeum
                #:fmt
                #:href
                #:dict
                #:->)
  (:import-from #:str
                #:split)
  (:import-from #:find-port
                #:find-port
                #:port-open-p)
  (:import-from #:alexandria
                #:write-string-into-file))
(in-package #:40ants-lisp-dev-mcp/main)


(defvar *opencode-config-pathname*
  #P"opencode.json")


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

(-> get-port-from-assistant-config ()
    (values (or null
                integer)
            &optional))

(defun get-port-from-assistant-config ()
  (let ((file (probe-file *opencode-config-pathname*)))
    (when file
      (let* ((data (read-config file))
             (url (href data "mcp" "lisp-dev-mcp" "url")))
        (when url
          (let ((third-part (third (split #\: url))))
            (when third-part
              (let ((port-as-str (first (split #\/ third-part))))
                (values (parse-integer port-as-str))))))))))


(-> update-port-in-config (integer)
    (values &optional))


(defun update-port-in-config (port)
  (let* ((file (probe-file *opencode-config-pathname*))
         (data (if file
                 (read-config file)
                 (make-default-config)))
         (url (fmt "http://localhost:~A/mcp"
                   port)))
    (setf (href data "mcp" "lisp-dev-mcp" "url")
          url)
    (write-config *opencode-config-pathname*
                  data)
    (values)))


(-> choose-port (string)
    (values integer boolean &optional))

(defun choose-port (port)
  "Returns a port as a first value and True if this port is not the same as in the current Opencode config."
  (let* ((port-from-assistant-config
           (get-port-from-assistant-config))
         (port-to-return
           (cond
             ((string-equal port
                            "auto")
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
              (let ((parsed (parse-integer port)))
                (unless (port-open-p parsed)
                  (error "Port ~A already taken by other program."
                         port))
                parsed)))))
    (values port-to-return
            (not
             (equal port-to-return
                    port-from-assistant-config)))))


(defmain (main) ((port "TCP port to listen on. If given, Streaming HTTP transport will be used. If \"auto\" then port will be choosen automatically.")
                 (debug "If this flag set, then a debugger will be opened when you've conntected to the server with SLY."
                        :flag t)
                 (log-filename "Path to a file with log.")
                 (update-config "Write choosen port to opencode.json config."
                                :flag t)
                 (verbose "Show debug messages in the log."
                          :flag t))
  "Main entry point for the Roswell script"
  (let ((log-level
          (if verbose
            :debug
            :info)))
    (cond
      (log-filename
       (40ants-logging:setup-for-backend
        :filename (uiop:ensure-pathname log-filename)
        :level log-level))
      (t
       (40ants-logging:setup-for-cli
        :level log-level))))

  (log:config '(40ants-slynk) :warn)
  (log:config '(sento actor-system) :warn)

  ;; Start SLYNK server if SLYNK_PORT environment variable is set
  (start-slynk-if-needed)

  (when debug
    (setf jsonrpc/errors:*debug-on-error* t))

  (multiple-value-bind (port port-differ-from-opencode-config)
      (when port
        (choose-port port))

    (when (and port
               port-differ-from-opencode-config
               update-config)
      (update-port-in-config port))
    
    (40ants-lisp-dev-mcp/core:start-server
     :port port
     :in-thread nil)))
