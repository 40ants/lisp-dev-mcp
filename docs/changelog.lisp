(uiop:define-package #:40ants-lisp-dev-mcp-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-lisp-dev-mcp-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"
                              "CST"
                              "AST"
                              "JSON"
                              "MCP"
                              "LLM"))
  (0.2.0 2026-03-06
         "* Added paredit-style structural editing tools (wrap, unwrap, raise, slurp, barf, kill, transpose, split, join).
          * Added sexp-replace, sexp-insert-before, sexp-insert-after for code replacement and insertion.
          * Added sexp-show-structure and sexp-get-enclosing for code inspection.
          * Added sexp-to-json-ast for CST-based JSON AST output.
          * Added Eclector-based CST parser with source position tracking.")
  (0.1.0 2026-01-25
         "* Initial version."))
