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
  (:import-from #:str)
  (:export #:parse-agents))
(in-package #:40ants-lisp-dev-mcp/main)


(defun parse-agents (value)
  "Parses comma-separated agent names into a list of :OPENCODE and :CODEX.
Names are case-insensitive and may contain surrounding whitespace. NIL uses
the default (:OPENCODE). Unknown names and empty elements signal an error."
  (if (null value)
      (list :opencode)
      (remove-duplicates
       (loop for name in (str:split "," value :omit-nulls nil)
             for trimmed = (str:trim name)
             collect (cond
                       ((string-equal trimmed "opencode") :opencode)
                       ((string-equal trimmed "codex") :codex)
                       (t (error "Unknown agent ~S; expected opencode or codex." trimmed))))
       :from-end t)))


(defmain (main) ((port "TCP port to listen on. If given, Streaming HTTP transport will be used. If \"auto\" then port will be choosen automatically.")
                 (debug "If this flag set, then a debugger will be opened when you've conntected to the server with SLY."
                        :flag t)
                 (log-filename "Path to a file with log.")
                 (agents "Comma-separated agents: opencode,codex. Defaults to opencode.")
                 (update-config "Write the chosen port to the selected agents' configs."
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

  (start-server :port (cond
                        ((null port) nil)
                        ((string-equal port "auto") :auto)
                        (t (parse-integer port)))
                :update-config update-config
                :agents (parse-agents agents)
                :in-thread nil))
