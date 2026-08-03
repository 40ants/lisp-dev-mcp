(uiop:define-package #:40ants-lisp-dev-mcp-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-lisp-dev-mcp-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.0 2026-08-02
         "* `start-server` learned to pick a free port automatically when `:PORT` is given as `:AUTO`, and to write it into `opencode.json` via the new `:UPDATE-CONFIG` argument, so it is now usable from the REPL or other programs.
* Exported `choose-port`, `update-port-in-config`, `get-port-from-assistant-config`, `*opencode-config-pathname*` and the config helpers.")
  (0.1.0 2026-01-25
         "* Initial version."))
