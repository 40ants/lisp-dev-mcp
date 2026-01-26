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
  (:import-from #:log))
(in-package #:40ants-lisp-dev-mcp/main)


(defmain (main) ((port "TCP port to listen on. If given, Streaming HTTP transport will be used.")
                 (debug "If this flag set, then a debugger will be opened when you've conntected to the server with SLY."
                        :flag t)
                 (log-filename "Path to a file with log.")
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
  
  (40ants-lisp-dev-mcp/core:start-server
   :port (when port
           (parse-integer port))
   :in-thread nil))
