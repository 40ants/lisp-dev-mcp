(uiop:define-package #:40ants-lisp-dev-mcp-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-lisp-dev-mcp-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"
                              "MCP"
                              "CLI"
                              "JSON"
                              "TOML"))
  (0.3.0 2026-10-08
         "* Added Codex config support in `.codex/config.toml`, including creation of missing files, directories and MCP sections. Existing enabled and timeout values are preserved; absent values default to true, 10 and 60 seconds.
* Added `:AGENTS` to `start-server` and `--agents` to the CLI; the default remains OpenCode.
* Automatic port selection checks selected agents in order, and config updates now include every selected agent even when the port is reused.
* Config parse or serialization errors abort updates before any selected config is written.
* OpenCode config updates preserve JSON arrays, including empty arrays.")
  (0.2.0 2026-08-02
         "* `start-server` learned to pick a free port automatically when `:PORT` is given as `:AUTO`, and to write it into `opencode.json` via the new `:UPDATE-CONFIG` argument, so it is now usable from the REPL or other programs.
* Exported `choose-port`, `update-port-in-config`, `get-port-from-assistant-config`, `*opencode-config-pathname*` and the config helpers.")
  (0.1.0 2026-01-25
         "* Initial version."))
