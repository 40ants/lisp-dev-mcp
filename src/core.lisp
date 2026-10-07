(uiop:define-package #:40ants-lisp-dev-mcp/core
  (:use #:cl)
  (:import-from #:40ants-mcp)
  (:import-from #:40ants-logging)
  (:import-from #:serapeum
                #:fmt
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
  (:import-from #:cl-toml)
  (:import-from #:str
                #:split)
  (:import-from #:find-port
                #:find-port
                #:port-open-p)
  (:export #:start-server
           #:*opencode-config-pathname*
           #:*codex-config-pathname*
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

You can rebind this variable, pass :OPENCODE-CONFIG to START-SERVER,
or pass :CONFIG to the config helpers and CHOOSE-PORT.")


(defvar *codex-config-pathname*
  #P".codex/config.toml"
  "Pathname of the Codex config, relative to the current working directory.
Rebind this variable or pass :CODEX-CONFIG to START-SERVER and CHOOSE-PORT,
or :CONFIG with :AGENT :CODEX to the config helpers.")


(defun agent-config-pathname (agent)
  (ecase agent
    (:opencode *opencode-config-pathname*)
    (:codex *codex-config-pathname*)))


(defun normalize-agents (agents)
  (unless (alexandria:proper-list-p agents)
    (error "AGENTS must be a proper list of keywords, got ~S." agents))
  (dolist (agent agents)
    (agent-config-pathname agent))
  (remove-duplicates agents :from-end t))


(-> read-config (pathname)
    (values hash-table &optional))

(defun read-config (path)
  "Reads the OpenCode JSON config at PATH, preserving arrays, booleans and nulls."
  (yason:parse path
               :json-arrays-as-vectors t
               :json-booleans-as-symbols t
               :json-nulls-as-keyword t))


(-> write-config (pathname hash-table)
    (values &optional))

(defun write-config (file data)
  "Writes DATA as an OpenCode JSON config to FILE."
  (let ((content (yason:with-output-to-string* (:indent 4)
                   (yason:encode data))))
    (write-string-into-file content
                            file
                            :if-exists :supersede)
    (values)))


(-> make-default-config ()
    (values hash-table &optional))

(defun make-default-config ()
  "Returns the default OpenCode config with a placeholder MCP URL."
  (dict "$schema" "https://opencode.ai/config.json"
        "skills" (dict "paths"
                       #(".agents/skills"))
        "mcp" (dict "lisp-dev-mcp"
                    (dict "type" "remote"
                          "url" "to be replaced"))))


(defun read-agent-config (agent path)
  (ecase agent
    (:opencode (read-config path))
    (:codex (cl-toml:parse-file path))))


(defun serialize-agent-config (agent data)
  (ecase agent
    (:opencode (yason:with-output-to-string* (:indent 4)
                 (yason:encode data)))
    (:codex (with-output-to-string (stream)
              (cl-toml:encode data stream)))))


(defun mcp-server-config (agent data &key create)
  (labels ((table (parent key)
             (multiple-value-bind (value presentp) (gethash key parent)
               (cond
                 (presentp
                  (unless (hash-table-p value)
                    (error "Expected a table at ~S in the ~S config, got ~S."
                           key agent value))
                  value)
                 (create (setf (gethash key parent) (dict)))))))
    (let ((servers (table data (ecase agent
                                (:opencode "mcp")
                                (:codex "mcp_servers")))))
      (when servers
        (table servers "lisp-dev-mcp")))))


(-> get-port-from-assistant-config (&key (:agent keyword) (:config pathname))
    (values (or null integer) &optional))

(defun get-port-from-assistant-config (&key (agent :opencode)
                                          (config (agent-config-pathname agent)))
  "Returns the MCP port recorded in CONFIG for AGENT, or NIL.
AGENT is :OPENCODE (the default) or :CODEX; CONFIG defaults to that agent's pathname."
  (agent-config-pathname agent)
  (let ((file (probe-file config)))
    (when file
      (let* ((data (read-agent-config agent file))
             (server (mcp-server-config agent data))
             (url (when server (gethash "url" server))))
        (when url
          (let ((third-part (third (split #\: url))))
            (when third-part
              (let ((port-as-str (first (split #\/ third-part))))
                (values (parse-integer port-as-str))))))))))


(defun prepare-config-update (port agent config)
  (let* ((file (probe-file config))
         (data (if file
                   (read-agent-config agent file)
                   (ecase agent
                     (:opencode (make-default-config))
                     (:codex (dict)))))
         (server (mcp-server-config agent data :create t)))
    (setf (gethash "url" server) (fmt "http://localhost:~A/mcp" port))
    (when (eq agent :codex)
      (loop for (key value) on '("enabled" cl-toml:true
                                "startup_timeout_sec" 10
                                "tool_timeout_sec" 60) by #'cddr
            do (unless (nth-value 1 (gethash key server))
                 (setf (gethash key server) value))))
    (serialize-agent-config agent data)))


(defun write-config-content (path content)
  (ensure-directories-exist path)
  (write-string-into-file content path :if-exists :supersede)
  (values))


(-> update-port-in-config (integer &key (:agent keyword) (:config pathname))
    (values &optional))

(defun update-port-in-config (port &key (agent :opencode)
                                      (config (agent-config-pathname agent)))
  "Writes PORT into the MCP URL in CONFIG for AGENT (:OPENCODE or :CODEX).
Creates missing directories and config tables. Codex gets enabled=true and
timeouts of 10 and 60 seconds when those settings are absent; existing values
are preserved. TOML comments and formatting are not preserved."
  (agent-config-pathname agent)
  (write-config-content config (prepare-config-update port agent config)))


(-> choose-port ((or integer (eql :auto) string)
                &key (:config pathname) (:codex-config pathname) (:agents list))
    (values integer boolean &optional))

(defun choose-port (port &key (config *opencode-config-pathname*)
                             (codex-config *codex-config-pathname*)
                             (agents '(:opencode)))
  "Resolves PORT into a concrete TCP port number and returns it as the first value.

As the second value returns T when the resolved port differs from at least one
selected agent's recorded port (including a missing config).

   PORT can be:
     - an INTEGER, used as-is after checking it is free;
     - the :AUTO keyword (or the string \"auto\"), in which case a free port
       is selected automatically, reusing the first free recorded port in
       AGENTS order. AGENTS defaults to (:OPENCODE); NIL skips reading configs.
       CONFIG points to OpenCode, CODEX-CONFIG points to Codex."
  (let* ((agents (normalize-agents agents))
         (configured-ports
           (loop for agent in agents
                 collect (get-port-from-assistant-config
                          :agent agent
                          :config (ecase agent
                                    (:opencode config)
                                    (:codex codex-config)))))
         (port-to-return
           (cond
             ((or (eql port :auto)
                  (and (stringp port)
                       (string-equal port "auto")))
              (or (find-if (lambda (configured-port)
                             (and configured-port (port-open-p configured-port)))
                           configured-ports)
                  (find-port)))
             (t
              (let ((parsed (etypecase port
                              (integer port)
                              (string (parse-integer port)))))
                (unless (port-open-p parsed)
                  (error "Port ~A already taken by other program."
                         parsed))
                parsed)))))
    (values port-to-return
            (some (lambda (configured-port)
                    (not (eql port-to-return configured-port)))
                  configured-ports))))


(defun start-server (&key port (in-thread t) update-config (agents '(:opencode))
                         (opencode-config *opencode-config-pathname*)
                         (codex-config *codex-config-pathname*))
  "Starts the MCP server.

   PORT controls the transport and the port:
     - NIL (the default) uses the stdio transport;
     - an INTEGER uses the Streaming HTTP transport on that TCP port;
     - :AUTO selects a free TCP port automatically, reusing the port from
       the selected agents' configs in AGENTS order when it is still available.

   IN-THREAD controls whether the server runs in a background thread (the
   default) or blocks the caller.

   When UPDATE-CONFIG is true and a port was selected (or reused), the chosen
   port is written into every selected agent's config, creating missing files
   and directories. AGENTS is a list of :OPENCODE and :CODEX, defaulting to
   (:OPENCODE); NIL disables config reads and updates. OPENCODE-CONFIG and
   CODEX-CONFIG default to *OPENCODE-CONFIG-PATHNAME* and *CODEX-CONFIG-PATHNAME*.
   Codex settings other than the URL are preserved; missing enabled and timeout
   settings receive defaults. TOML comments and formatting may change.

   Returns the server thread when IN-THREAD is true, otherwise blocks."
  (let* ((agents (normalize-agents agents))
         (chosen-port (when port
                        (choose-port port :config opencode-config
                                          :codex-config codex-config :agents agents))))
    (when (and chosen-port update-config)
      ;; Prepare every file before writing any, so parse/encode failures cannot
      ;; leave the selected agents with partially updated configuration.
      (let ((updates
              (loop for agent in agents
                    for path = (ecase agent
                                 (:opencode opencode-config)
                                 (:codex codex-config))
                    collect (cons path (prepare-config-update chosen-port agent path)))))
        (loop for (path . content) in updates
              do (write-config-content path content))))
    
    (flet ((server-fn ()
             (40ants-mcp/server/definition:start-server dev-tools
                                                        :transport (if chosen-port
                                                                     :http
                                                                     :stdio)
                                                        :port chosen-port)))
      (if in-thread
        (make-thread #'server-fn :name "MCP Server Thread")
        (funcall #'server-fn)))))
