#-asdf3.1 (error "40ants-lisp-dev-mcp requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "40ants-lisp-dev-mcp"
  :description "MCP which gives LLM tools for working with running Lisp image."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants/lisp-dev-mcp/"
  :source-control (:git "https://github.com/40ants/lisp-dev-mcp")
  :bug-tracker "https://github.com/40ants/lisp-dev-mcp/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("defmain"
               "40ants-mcp"
               "40ants-logging"
               "40ants-slynk"
               "serapeum"
               "alexandria"
               "trivial-backtrace"
               "str"
               "openrpc-server"
               "jsonrpc/errors"
               "bordeaux-threads"
               "40ants-lisp-dev-mcp/core")
  :in-order-to ((test-op (test-op "40ants-lisp-dev-mcp-tests"))))


(asdf:register-system-packages "log4cl" '("LOG"))