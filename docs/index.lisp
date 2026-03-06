(uiop:define-package #:40ants-lisp-dev-mcp-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:40ants-lisp-dev-mcp-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-lisp-dev-mcp-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-lisp-dev-mcp-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "40ants-lisp-dev-mcp - MCP which gives LLM tools for working with running Lisp image."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "LLM"
                                   "MCP"
                                   "SLYNK"
                                   "IDE"
                                   "TCP"
                                   "API"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "GIT"
                                   "CST"
                                   "AST"
                                   "S-expression"))
  (40ants-lisp-dev-mcp system)
  "
[![](https://github-actions.40ants.com/40ants/lisp-dev-mcp/matrix.svg?only=ci.run-tests)](https://github.com/40ants/lisp-dev-mcp/actions)

![Quicklisp](http://quickdocs.org/badge/40ants-lisp-dev-mcp.svg)
"
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```

then:

```
ros install 40ants/lisp-dev-mcp
```
""")


(defsection @usage (:title "Usage")
  """

# Running in stdio mode

Here is an example config to add lisp-dev-mcp to Qwen:

```
{
  "mcpServers": {
    "lisp-dev": {
      "command": "lisp-dev-mcp",
      "args": []
    }
  },
  "$version": 2
}
```

If you want to debug MCP server, then you might start it will logging output and a SLYNK port opened:

```
{
  "mcpServers": {
    "lisp-dev": {
      "command": "lisp-dev-mcp",
      "args": ["--log", "mcp.log", "--verbose"],
      "env": {
        "SLYNK_PORT": "9991"
      }
    }
  },
  "$version": 2
}
```

# Running in HTTP streaming mode

Start the lisp process:

```
qlot exec roswell/lisp-dev-mcp.ros --port 7890
```

or in the REPL:

```
(ql:quickload :40ants-lisp-dev-mcp)

(40ants-lisp-dev-mcp/core:start-server :port 7890)
```

then configure your IDE:

```
{
  "mcpServers": {
    "lisp-dev": {
      "url": "http://localhost:7890/mcp"
    }
  },
  "$version": 2
}
```

""")


(defautodoc @api (:system "40ants-lisp-dev-mcp"))
