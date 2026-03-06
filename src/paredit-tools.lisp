(uiop:define-package #:40ants-lisp-dev-mcp/paredit-tools
  (:use #:cl)
  (:import-from #:40ants-mcp/tools
                #:define-tool)
  (:import-from #:40ants-mcp/content/text
                #:text-content)
  (:import-from #:40ants-mcp/server/errors
                #:tool-error)
  (:import-from #:40ants-lisp-dev-mcp/core
                #:dev-tools)
  (:import-from #:40ants-lisp-dev-mcp/cst
                #:parse-top-level-forms
                #:make-cst-node)
  (:import-from #:40ants-lisp-dev-mcp/paredit
                #:resolve-target
                #:transform-wrap
                #:transform-unwrap
                #:transform-raise
                #:transform-slurp-forward
                #:transform-slurp-backward
                #:transform-barf-forward
                #:transform-barf-backward
                #:transform-kill
                #:transform-transpose
                #:transform-split
                #:transform-join
                #:transform-replace
                #:transform-insert-before
                #:transform-insert-after
                #:show-structure
                #:get-enclosing
                #:cst-to-json-ast
                #:paredit-error
                #:node-source
                #:node-list-p
                #:find-parent))
(in-package #:40ants-lisp-dev-mcp/paredit-tools)


;;; Shared helpers for all paredit tools

(defun parse-path-string (path-str)
  "Parse a comma-separated path string like \"2,0,1\" into a list of integers."
  (when (and path-str (plusp (length path-str)))
    (mapcar #'parse-integer
            (uiop:split-string path-str :separator ","))))

(defun apply-paredit-tool (code form-type form-name target path line
                           transform-fn &rest transform-args)
  "Parse CODE, resolve target, apply TRANSFORM-FN, return new text.
   Handles empty code for insert operations."
  (handler-case
      (if (and (or (null code) (string= code ""))
               (null target) (null path))
          ;; Empty input: create a synthetic empty root for insert operations
          (let* ((root (make-cst-node :kind :expr :value nil
                                      :start 0 :end 0))
                 (new-text (apply transform-fn "" root root transform-args)))
            new-text)
          (let* ((text (or code ""))
                 (nodes (parse-top-level-forms text)))
            (when (null nodes)
              (error 'paredit-error
                     :format-control "No forms found in code"
                     :format-arguments nil))
            (multiple-value-bind (target-node root-node)
                (resolve-target nodes text
                                :form-type form-type
                                :form-name form-name
                                :target target
                                :path (parse-path-string path)
                                :line line)
              (apply transform-fn text root-node target-node transform-args))))
    (paredit-error (e)
      (error 'tool-error
             :content (list (make-instance 'text-content
                                           :text (format nil "~A" e)))))))


;;; Tool definitions — all registered under the dev-tools API

(define-tool (dev-tools sexp-wrap) (code &key form-type form-name target path line
                                         (count 1) wrapper head)
  (:summary "Wrap one or more S-expressions in new delimiters.

             Wraps the targeted expression (and optionally COUNT-1 following
             siblings) in parentheses. Use WRAPPER to choose delimiter type
             (round, square, curly). Use HEAD to prepend a symbol inside.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to (e.g. \"defun\").")
  (:param form-name string "Name of top-level form (e.g. \"foo\"). Supports [N] index.")
  (:param target string "Source text of the target S-expression.")
  (:param path string "Child index path, comma-separated (e.g. \"2,0,1\").")
  (:param line integer "Line number hint for disambiguation.")
  (:param count integer "Number of siblings to wrap (default 1).")
  (:param wrapper string "Delimiter type: round, square, curly (default round).")
  (:param head string "Symbol to prepend inside the wrapper (e.g. \"progn\").")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-wrap
                                                 :count count
                                                 :wrapper wrapper
                                                 :head head))))


(define-tool (dev-tools sexp-unwrap) (code &key form-type form-name target path line
                                           (keep "all"))
  (:summary "Remove list delimiters and splice children into parent.

             Use KEEP=\"body\" to also drop the operator (first child).")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the target list.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param keep string "\"all\" keeps all children, \"body\" drops operator.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-unwrap
                                                 :keep keep))))


(define-tool (dev-tools sexp-raise) (code &key form-type form-name target path line)
  (:summary "Replace parent with the targeted child expression.

             The targeted expression replaces its enclosing list.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the expression to raise.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-raise))))


(define-tool (dev-tools sexp-slurp-forward) (code &key form-type form-name
                                                   target path line (count 1))
  (:summary "Pull following sibling(s) into the targeted list.

             The closing delimiter moves right to absorb COUNT siblings.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the target list.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param count integer "Number of siblings to slurp (default 1).")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-slurp-forward
                                                 :count count))))


(define-tool (dev-tools sexp-slurp-backward) (code &key form-type form-name
                                                    target path line (count 1))
  (:summary "Pull preceding sibling(s) into the targeted list.

             The opening delimiter moves left to absorb COUNT siblings.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the target list.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param count integer "Number of siblings to slurp (default 1).")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-slurp-backward
                                                 :count count))))


(define-tool (dev-tools sexp-barf-forward) (code &key form-type form-name
                                                  target path line (count 1))
  (:summary "Push last child(ren) out of the targeted list as following siblings.

             The closing delimiter moves left, ejecting COUNT trailing children.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the target list.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param count integer "Number of children to barf out (default 1).")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-barf-forward
                                                 :count count))))


(define-tool (dev-tools sexp-barf-backward) (code &key form-type form-name
                                                   target path line (count 1))
  (:summary "Push first child(ren) out of the targeted list as preceding siblings.

             The opening delimiter moves right, ejecting COUNT leading children.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the target list.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param count integer "Number of children to barf out (default 1).")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-barf-backward
                                                 :count count))))


(define-tool (dev-tools sexp-kill) (code &key form-type form-name target path line
                                         (count 1))
  (:summary "Delete one or more complete S-expressions.

             Removes the targeted expression and cleans up surrounding whitespace.
             Use COUNT to kill multiple consecutive siblings.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the expression to kill.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param count integer "Number of siblings to kill (default 1).")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-kill
                                                 :count count))))


(define-tool (dev-tools sexp-transpose) (code &key form-type form-name
                                               target path line)
  (:summary "Swap the targeted expression with its next sibling.

             The two adjacent siblings exchange positions.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the expression to transpose.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-transpose))))


(define-tool (dev-tools sexp-split) (code &key form-type form-name target path line
                                          clone-head)
  (:summary "Split the parent list at the targeted expression.

             The parent list is split into two lists. TARGET becomes the first
             element of the second list. Use CLONE-HEAD to copy the operator
             to both halves.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the split point expression.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param clone-head boolean "If true, copy operator to both halves.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-split
                                                 :clone-head clone-head))))


(define-tool (dev-tools sexp-join) (code &key form-type form-name target path line
                                         drop-head)
  (:summary "Join the targeted list with its next sibling list.

             Merges two adjacent lists into one. Use DROP-HEAD to discard
             the second list's operator.")
  (:param code string "Lisp source code to transform.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the first list to join.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param drop-head boolean "If true, discard the second list's operator.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-join
                                                 :drop-head drop-head))))


(define-tool (dev-tools sexp-replace) (code new-code &key form-type form-name
                                                      target path line)
  (:summary "Replace the targeted S-expression with new code.

             The matched node is replaced with the provided NEW-CODE string.")
  (:param code string "Lisp source code to transform.")
  (:param new-code string "Replacement source code.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the expression to replace.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-replace
                                                 :new-code new-code))))


(define-tool (dev-tools sexp-insert-before) (code new-code &key form-type form-name
                                                             target path line)
  (:summary "Insert new code before the targeted expression.

             When no target is specified, prepends to the beginning of the code.
             Works with empty code input to build files from scratch.")
  (:param code string "Lisp source code to transform (can be empty).")
  (:param new-code string "New source code to insert.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the reference expression.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-insert-before
                                                 :new-code new-code))))


(define-tool (dev-tools sexp-insert-after) (code new-code &key form-type form-name
                                                            target path line)
  (:summary "Insert new code after the targeted expression.

             When no target is specified, appends to the end of the code.
             Works with empty code input to build files from scratch.")
  (:param code string "Lisp source code to transform (can be empty).")
  (:param new-code string "New source code to insert.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of the reference expression.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content
                       :text (apply-paredit-tool code form-type form-name
                                                 target path line
                                                 #'transform-insert-after
                                                 :new-code new-code))))


(define-tool (dev-tools sexp-show-structure) (code &key form-type form-name
                                                    target path line
                                                    (depth 4) (max-text 60))
  (:summary "Show the structure tree of the code or targeted expression.

             Returns an indented tree of paths, node kinds, line numbers,
             and text snippets. Read-only — does not modify the code.")
  (:param code string "Lisp source code to inspect.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of expression to inspect.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param depth integer "Maximum tree depth to display (default 4).")
  (:param max-text integer "Maximum text length per node (default 60).")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((text (or code ""))
             (nodes (parse-top-level-forms text)))
        (multiple-value-bind (target-node root-node)
            (resolve-target nodes text
                            :form-type form-type
                            :form-name form-name
                            :target target
                            :path (parse-path-string path)
                            :line line)
          (declare (ignore root-node))
          (list (make-instance 'text-content
                               :text (show-structure target-node text
                                                     :depth depth
                                                     :max-text max-text)))))
    (paredit-error (e)
      (error 'tool-error
             :content (list (make-instance 'text-content
                                           :text (format nil "~A" e)))))))


(define-tool (dev-tools sexp-to-json-ast) (code &key form-type form-name
                                                 (depth 6) (max-text 120))
  (:summary "Parse Lisp code and return a JSON AST (Abstract Syntax Tree).

             Returns a JSON array of top-level form objects. Each object has:
             type (list/symbol/string/number/atom), path (index array),
             line, start, end, text, and for lists: head (operator name)
             and children (nested array of the same structure).

             This is the preferred way for LLMs to understand code structure
             before applying structural edits. The path arrays in the output
             can be used directly as the 'path' parameter in other sexp- tools.")
  (:param code string "Lisp source code to parse.")
  (:param form-type string "Type of top-level form to scope to (e.g. \"defun\").")
  (:param form-name string "Name of top-level form (e.g. \"foo\").")
  (:param depth integer "Maximum tree depth (default 6).")
  (:param max-text integer "Maximum text length per node (default 120).")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((text (or code ""))
             (nodes (parse-top-level-forms text))
             (scoped-nodes
               (if (and form-type form-name)
                   (multiple-value-bind (target-node root-node)
                       (resolve-target nodes text
                                       :form-type form-type
                                       :form-name form-name)
                     (declare (ignore root-node))
                     (list target-node))
                   nodes)))
        (list (make-instance 'text-content
                             :text (cst-to-json-ast scoped-nodes text
                                                    :depth depth
                                                    :max-text max-text))))
    (paredit-error (e)
      (error 'tool-error
             :content (list (make-instance 'text-content
                                           :text (format nil "~A" e)))))))


(define-tool (dev-tools sexp-get-enclosing) (code &key form-type form-name
                                                   target path line (levels 1))
  (:summary "Return info about the enclosing form of the targeted expression.

             Shows the head symbol, kind, child index, sibling count, line
             number, and text of the enclosing form. Read-only.")
  (:param code string "Lisp source code to inspect.")
  (:param form-type string "Type of top-level form to scope to.")
  (:param form-name string "Name of top-level form.")
  (:param target string "Source text of expression to find enclosing for.")
  (:param path string "Child index path, comma-separated.")
  (:param line integer "Line number hint.")
  (:param levels integer "Number of enclosing levels to traverse (default 1).")
  (:result (soft-list-of text-content))
  (handler-case
      (let* ((text (or code ""))
             (nodes (parse-top-level-forms text)))
        (multiple-value-bind (target-node root-node)
            (resolve-target nodes text
                            :form-type form-type
                            :form-name form-name
                            :target target
                            :path (parse-path-string path)
                            :line line)
          (list (make-instance 'text-content
                               :text (get-enclosing root-node target-node text
                                                    :levels levels)))))
    (paredit-error (e)
      (error 'tool-error
             :content (list (make-instance 'text-content
                                           :text (format nil "~A" e)))))))
