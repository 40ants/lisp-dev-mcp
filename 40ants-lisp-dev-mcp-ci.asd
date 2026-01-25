(defsystem "40ants-lisp-dev-mcp-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants/lisp-dev-mcp/"
  :class :package-inferred-system
  :description "Provides CI settings for 40ants-lisp-dev-mcp."
  :source-control (:git "https://github.com/40ants/lisp-dev-mcp")
  :bug-tracker "https://github.com/40ants/lisp-dev-mcp/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "40ants-lisp-dev-mcp-ci/ci"))
