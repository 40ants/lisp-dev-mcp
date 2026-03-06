(defsystem "40ants-lisp-dev-mcp-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants/lisp-dev-mcp/"
  :class :package-inferred-system
  :description "Provides tests for 40ants-lisp-dev-mcp."
  :source-control (:git "https://github.com/40ants/lisp-dev-mcp")
  :bug-tracker "https://github.com/40ants/lisp-dev-mcp/issues"
  :pathname "t"
  :depends-on ("40ants-lisp-dev-mcp-tests/core"
               "40ants-lisp-dev-mcp-tests/paredit")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
