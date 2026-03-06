(uiop:define-package #:40ants-lisp-dev-mcp-tests/paredit
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:ng
                #:signals
                #:testing)
  (:import-from #:40ants-lisp-dev-mcp/cst
                #:cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:parse-top-level-forms)
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
(in-package #:40ants-lisp-dev-mcp-tests/paredit)


;;;; Test helpers

(defun parse-single (text)
  "Parse TEXT and return the single top-level CST node."
  (let ((nodes (parse-top-level-forms text)))
    (assert (= 1 (length nodes)))
    (first nodes)))

(defun resolve-and-transform (text transform-fn &key target path
                                                     form-type form-name line
                                                     &allow-other-keys)
  "Parse TEXT, resolve target, apply TRANSFORM-FN with remaining keyword args."
  (let* ((nodes (parse-top-level-forms text))
         (path-list (when path
                      (etypecase path
                        (list path)
                        (string (mapcar #'parse-integer
                                        (uiop:split-string path :separator ",")))))))
    (multiple-value-bind (target-node root-node)
        (resolve-target nodes text
                        :form-type form-type :form-name form-name
                        :target target :path path-list :line line)
      (values target-node root-node))))

(defun do-transform (text transform-fn target-spec &rest transform-args)
  "Convenience: parse, resolve by text match, transform.
   TARGET-SPEC is the source text to match."
  (let* ((nodes (parse-top-level-forms text)))
    (multiple-value-bind (target-node root-node)
        (resolve-target nodes text :target target-spec)
      (apply transform-fn text root-node target-node transform-args))))

(defun do-transform-path (text transform-fn path &rest transform-args)
  "Convenience: parse, resolve by path, transform."
  (let* ((nodes (parse-top-level-forms text)))
    (multiple-value-bind (target-node root-node)
        (resolve-target nodes text :path path)
      (apply transform-fn text root-node target-node transform-args))))


;;;; CST parsing tests

(deftest test-parse-simple-atom ()
  (testing "Parse a single atom"
    (let ((node (parse-single "hello")))
      (ok (eq (cst-node-kind node) :expr))
      (ok (symbolp (cst-node-value node)))
      (ok (= 0 (cst-node-start node)))
      (ok (= 5 (cst-node-end node))))))

(deftest test-parse-number ()
  (testing "Parse a number"
    (let ((node (parse-single "42")))
      (ok (eq (cst-node-kind node) :expr))
      (ok (= 42 (cst-node-value node)))
      (ok (= 0 (cst-node-start node)))
      (ok (= 2 (cst-node-end node))))))

(deftest test-parse-simple-list ()
  (testing "Parse a simple list"
    (let ((node (parse-single "(a b c)")))
      (ok (node-list-p node))
      (ok (= 0 (cst-node-start node)))
      (ok (= 7 (cst-node-end node)))
      (ok (= 1 (cst-node-start-line node))))))

(deftest test-parse-nested-list ()
  (testing "Parse nested lists"
    (let* ((text "(defun foo (x) (+ x 1))")
           (node (parse-single text)))
      (ok (node-list-p node))
      (ok (string= (node-source text node) text)))))

(deftest test-parse-multiple-forms ()
  (testing "Parse multiple top-level forms"
    (let ((nodes (parse-top-level-forms "(a) (b) (c)")))
      (ok (= 3 (length nodes)))
      (ok (every #'node-list-p nodes)))))

(deftest test-parse-string-literal ()
  (testing "Parse string literal"
    (let* ((text "\"hello world\"")
           (node (parse-single text)))
      (ok (stringp (cst-node-value node)))
      (ok (string= "hello world" (cst-node-value node))))))

(deftest test-parse-preserves-positions ()
  (testing "Source positions are correct for nested forms"
    (let* ((text "(+ 1 (* 2 3))")
           (root (parse-single text))
           (children (remove-if-not
                      (lambda (c) (and (typep c 'cst-node)
                                       (eq (cst-node-kind c) :expr)))
                      (cst-node-children root))))
      ;; (+ 1 (* 2 3))
      ;;  0123456789...
      (ok (= 3 (length children)))
      ;; "+"
      (ok (= 1 (cst-node-start (first children))))
      (ok (= 2 (cst-node-end (first children))))
      ;; "1"
      (ok (= 3 (cst-node-start (second children))))
      (ok (= 4 (cst-node-end (second children))))
      ;; "(* 2 3)"
      (ok (= 5 (cst-node-start (third children))))
      (ok (= 13 (cst-node-end (third children)))))))


;;;; Target resolution tests

(deftest test-resolve-by-text ()
  (testing "Resolve target by source text"
    (let* ((text "(a (b c) d)")
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target root)
          (resolve-target nodes text :target "(b c)")
        (declare (ignore root))
        (ok (string= "(b c)" (node-source text target)))))))

(deftest test-resolve-by-path ()
  (testing "Resolve target by path"
    (let* ((text "(a (b c) d)")
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target root)
          (resolve-target nodes text :path '(1))
        (declare (ignore root))
        (ok (string= "(b c)" (node-source text target)))))))

(deftest test-resolve-by-form-type-name ()
  (testing "Resolve by form-type and form-name"
    (let* ((text "(defun foo () 42) (defun bar () 99)")
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target root)
          (resolve-target nodes text :form-type "defun" :form-name "bar")
        (ok (string= "(defun bar () 99)" (node-source text target)))
        (ok (eq target root))))))

(deftest test-resolve-not-found ()
  (testing "Error when target not found"
    (let* ((text "(a b c)")
           (nodes (parse-top-level-forms text)))
      (signals paredit-error
        (resolve-target nodes text :target "zzz")))))


;;;; Wrap tests (Emacs paredit: M-( wraps the following sexp)

(deftest test-wrap-single ()
  (testing "Wrap single expression"
    ;; (a b c) with target b -> (a (b) c)
    (ok (string= "(a (b) c)"
                 (do-transform "(a b c)" #'transform-wrap "b")))))

(deftest test-wrap-multiple ()
  (testing "Wrap multiple siblings"
    ;; (a b c d) wrapping b with count 2 -> (a (b c) d)
    (ok (string= "(a (b c) d)"
                 (do-transform "(a b c d)" #'transform-wrap "b" :count 2)))))

(deftest test-wrap-with-head ()
  (testing "Wrap with a head symbol"
    ;; (a b c) wrapping b with head "progn" -> (a (progn b) c)
    (ok (string= "(a (progn b) c)"
                 (do-transform "(a b c)" #'transform-wrap "b"
                               :head "progn")))))

(deftest test-wrap-square ()
  (testing "Wrap with square brackets"
    (ok (string= "(a [b] c)"
                 (do-transform "(a b c)" #'transform-wrap "b"
                               :wrapper "square")))))


;;;; Unwrap tests (Emacs paredit: M-s splices)

(deftest test-unwrap-all ()
  (testing "Unwrap keeping all children"
    ;; (a (b c) d) unwrapping (b c) -> (a b c d)
    (ok (string= "(a b c d)"
                 (do-transform "(a (b c) d)" #'transform-unwrap "(b c)")))))

(deftest test-unwrap-body ()
  (testing "Unwrap keeping body only (drop operator)"
    ;; (a (progn x y) d) unwrapping with keep=body -> (a x y d)
    (ok (string= "(a x y d)"
                 (do-transform "(a (progn x y) d)" #'transform-unwrap
                               "(progn x y)" :keep "body")))))

(deftest test-unwrap-atom-error ()
  (testing "Cannot unwrap an atom"
    (signals paredit-error
      (do-transform "(a b c)" #'transform-unwrap "b"))))


;;;; Raise tests (Emacs paredit: M-r raises)

(deftest test-raise ()
  (testing "Raise replaces parent with child"
    ;; (a (b c) d), raise c in (b c) -> (a c d)
    (ok (string= "(a c d)"
                 (do-transform "(a (b c) d)" #'transform-raise "c")))))

(deftest test-raise-nested ()
  (testing "Raise from deeply nested"
    ;; (+ (* 2 3) 4), raise (* 2 3) -> (* 2 3)
    (ok (string= "(* 2 3)"
                 (do-transform "(+ (* 2 3) 4)" #'transform-raise "(* 2 3)")))))


;;;; Slurp forward tests (Emacs paredit: C-) slurps forward)

(deftest test-slurp-forward ()
  (testing "Slurp one sibling forward"
    ;; (a (b) c d) slurp from (b) -> (a (b c) d)
    (ok (string= "(a (b c) d)"
                 (do-transform "(a (b) c d)" #'transform-slurp-forward "(b)")))))

(deftest test-slurp-forward-multiple ()
  (testing "Slurp two siblings forward"
    ;; (a (b) c d) slurp 2 from (b) -> (a (b c d))
    (ok (string= "(a (b c d))"
                 (do-transform "(a (b) c d)" #'transform-slurp-forward "(b)"
                               :count 2)))))

(deftest test-slurp-forward-no-sibling ()
  (testing "Cannot slurp when no sibling after"
    (signals paredit-error
      (do-transform "(a (b))" #'transform-slurp-forward "(b)"))))


;;;; Slurp backward tests (Emacs paredit: C-( slurps backward)

(deftest test-slurp-backward ()
  (testing "Slurp one sibling backward"
    ;; (a b (c) d) slurp backward from (c) -> (a (b c) d)
    (ok (string= "(a (b c) d)"
                 (do-transform "(a b (c) d)" #'transform-slurp-backward "(c)")))))

(deftest test-slurp-backward-no-sibling ()
  (testing "Cannot slurp backward when no preceding sibling"
    (signals paredit-error
      (do-transform "((a) b)" #'transform-slurp-backward "(a)"))))


;;;; Barf forward tests (Emacs paredit: C-} barfs forward)

(deftest test-barf-forward ()
  (testing "Barf last child forward"
    ;; (a (b c d) e) barf from (b c d) -> (a (b c) d e)
    (ok (string= "(a (b c) d e)"
                 (do-transform "(a (b c d) e)" #'transform-barf-forward
                               "(b c d)")))))

(deftest test-barf-forward-too-many ()
  (testing "Cannot barf more children than available"
    (signals paredit-error
      (do-transform "(a (b) c)" #'transform-barf-forward "(b)" :count 1))))


;;;; Barf backward tests (Emacs paredit: C-{ barfs backward)

(deftest test-barf-backward ()
  (testing "Barf first child backward"
    ;; (a (b c d) e) barf backward from (b c d) -> (a b (c d) e)
    (ok (string= "(a b (c d) e)"
                 (do-transform "(a (b c d) e)" #'transform-barf-backward
                               "(b c d)")))))


;;;; Kill tests (Emacs paredit: C-k kills sexp)

(deftest test-kill-middle ()
  (testing "Kill a middle expression"
    ;; (a b c) kill b -> (a c)
    (ok (string= "(a c)"
                 (do-transform "(a b c)" #'transform-kill "b")))))

(deftest test-kill-last ()
  (testing "Kill last expression"
    ;; (a b c) kill c -> (a b)
    (ok (string= "(a b)"
                 (do-transform "(a b c)" #'transform-kill "c")))))

(deftest test-kill-first ()
  (testing "Kill first expression"
    ;; (a b c) kill a -> (b c)
    (ok (string= "(b c)"
                 (do-transform "(a b c)" #'transform-kill "a")))))

(deftest test-kill-multiple ()
  (testing "Kill multiple consecutive siblings"
    ;; (a b c d) kill b with count 2 -> (a d)
    (ok (string= "(a d)"
                 (do-transform "(a b c d)" #'transform-kill "b" :count 2)))))


;;;; Transpose tests (Emacs paredit: C-M-t transposes)

(deftest test-transpose ()
  (testing "Transpose two siblings"
    ;; (a b c) transpose a and b -> (b a c)
    (ok (string= "(b a c)"
                 (do-transform "(a b c)" #'transform-transpose "a")))))

(deftest test-transpose-lists ()
  (testing "Transpose list expressions"
    ;; ((a b) (c d)) transpose -> ((c d) (a b))
    (ok (string= "((c d) (a b))"
                 (do-transform "((a b) (c d))" #'transform-transpose "(a b)")))))

(deftest test-transpose-no-next ()
  (testing "Cannot transpose last sibling"
    (signals paredit-error
      (do-transform "(a b c)" #'transform-transpose "c"))))


;;;; Split tests (Emacs paredit: M-S splits)

(deftest test-split ()
  (testing "Split list at target"
    ;; (a b c d) split at c -> (a b) (c d)
    (ok (string= "(a b) (c d)"
                 (do-transform "(a b c d)" #'transform-split "c")))))

(deftest test-split-clone-head ()
  (testing "Split with clone-head"
    ;; (progn a b c) split at b with clone -> (progn a) (progn b c)
    (ok (string= "(progn a) (progn b c)"
                 (do-transform "(progn a b c)" #'transform-split "b"
                               :clone-head t)))))


;;;; Join tests (Emacs paredit: M-J joins)

(deftest test-join ()
  (testing "Join two adjacent lists"
    ;; (a b) (c d) join -> (a b c d)
    ;; Need to target the first list within a wrapper
    (let* ((text "((a b) (c d))")
           (result (do-transform text #'transform-join "(a b)")))
      (ok (string= "((a b c d))" result)))))

(deftest test-join-drop-head ()
  (testing "Join with drop-head"
    (let* ((text "((progn a) (progn b))")
           (result (do-transform text #'transform-join "(progn a)"
                                 :drop-head t)))
      (ok (string= "((progn a b))" result)))))


;;;; Replace tests

(deftest test-replace-atom ()
  (testing "Replace an atom with new code"
    (ok (string= "(a NEW c)"
                 (do-transform "(a b c)" #'transform-replace "b"
                               :new-code "NEW")))))

(deftest test-replace-list ()
  (testing "Replace a list with new code"
    (ok (string= "(a (x y z) d)"
                 (do-transform "(a (b c) d)" #'transform-replace "(b c)"
                               :new-code "(x y z)")))))

(deftest test-replace-with-multiline ()
  (testing "Replace with multiline code"
    (let ((result (do-transform "(defun foo () nil)" #'transform-replace "nil"
                                :new-code "(let ((x 1))
  (+ x 2))")))
      (ok (search "(let ((x 1))" result)))))


;;;; Insert tests

(deftest test-insert-before ()
  (testing "Insert before a sibling"
    (ok (string= "(NEW a b c)"
                 (do-transform "(a b c)" #'transform-insert-before "a"
                               :new-code "NEW")))))

(deftest test-insert-after ()
  (testing "Insert after a sibling"
    (ok (string= "(a b c NEW)"
                 (do-transform "(a b c)" #'transform-insert-after "c"
                               :new-code "NEW")))))

(deftest test-insert-after-top-level ()
  (testing "Insert after at top level (append)"
    (let* ((text "(defun foo () 1)")
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target root)
          (resolve-target nodes text)
        (let ((result (transform-insert-after text root target
                                              :new-code "(defun bar () 2)")))
          (ok (search "(defun foo () 1)" result))
          (ok (search "(defun bar () 2)" result)))))))

(deftest test-insert-empty-code ()
  (testing "Insert into empty code (build from scratch)"
    (let* ((root (40ants-lisp-dev-mcp/cst:make-cst-node
                  :kind :expr :value nil :start 0 :end 0))
           (result (transform-insert-after "" root root
                                           :new-code "(defun foo () 42)")))
      (ok (string= "(defun foo () 42)" result)))))


;;;; Show-structure tests

(deftest test-show-structure ()
  (testing "Show structure returns tree text"
    (let* ((text "(defun foo (x) (+ x 1))")
           (root (parse-single text))
           (result (show-structure root text)))
      (ok (search "list" result))
      (ok (search "defun" result)))))


;;;; Get-enclosing tests

(deftest test-get-enclosing ()
  (testing "Get enclosing form info"
    (let* ((text "(defun foo (x) (+ x 1))")
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target root)
          (resolve-target nodes text :target "(+ x 1)")
        (let ((result (get-enclosing root target text)))
          (ok (search "defun" result))
          (ok (search "list" result)))))))


;;;; Round-trip tests

(deftest test-roundtrip-wrap-unwrap ()
  (testing "Wrap then unwrap is identity"
    (let* ((text "(a b c)")
           (wrapped (do-transform text #'transform-wrap "b"))
           (nodes2 (parse-top-level-forms wrapped)))
      (multiple-value-bind (target2 root2)
          (resolve-target nodes2 wrapped :target "(b)")
        (let ((unwrapped (transform-unwrap wrapped root2 target2)))
          (ok (string= text unwrapped)))))))

(deftest test-roundtrip-slurp-barf ()
  (testing "Slurp forward then barf forward is identity"
    (let* ((text "(a (b) c)")
           (slurped (do-transform text #'transform-slurp-forward "(b)"))
           (nodes2 (parse-top-level-forms slurped)))
      (multiple-value-bind (target2 root2)
          (resolve-target nodes2 slurped :target "(b c)")
        (let ((barfed (transform-barf-forward slurped root2 target2)))
          (ok (string= text barfed)))))))

(deftest test-roundtrip-transpose-twice ()
  (testing "Transpose twice returns to original"
    (let* ((text "(a b c)")
           ;; transpose a and b -> (b a c)
           (pass1 (do-transform text #'transform-transpose "a"))
           ;; transpose a and c -> (b c a)... no, need to transpose b with a again
           ;; Actually need to target "a" (now second) in (b a c)
           (nodes2 (parse-top-level-forms pass1)))
      ;; In (b a c), find "a" which is now second child, transpose with c
      ;; To get back to (a b c), we need to target "b" in (b a c)
      (multiple-value-bind (target2 root2)
          (resolve-target nodes2 pass1 :path '(0))
        (let ((pass2 (transform-transpose pass1 root2 target2)))
          ;; (b a c) -> transpose b with a -> (a b c)
          (ok (string= text pass2)))))))


;;;; Emacs paredit-inspired scenarios

(deftest test-paredit-scenario-wrap-function-call ()
  (testing "Wrap a value in a function call: x -> (1+ x)"
    (let ((result (do-transform "(setf x 5)" #'transform-wrap "5"
                                :head "1+")))
      (ok (string= "(setf x (1+ 5))" result)))))

(deftest test-paredit-scenario-extract-to-let ()
  (testing "Complex scenario: wrap expression for let binding"
    (let* ((text "(defun foo () (+ 1 2))")
           ;; Wrap (+ 1 2) with let
           (result (do-transform text #'transform-wrap "(+ 1 2)"
                                 :head "let ((x 42))")))
      (ok (search "let ((x 42))" result))
      (ok (search "(+ 1 2)" result)))))

(deftest test-paredit-scenario-build-file ()
  (testing "Build a file from scratch using insert"
    (let* ((code "")
           ;; Step 1: create the package definition
           (root1 (40ants-lisp-dev-mcp/cst:make-cst-node
                   :kind :expr :value nil :start 0 :end 0))
           (code (transform-insert-after code root1 root1
                                         :new-code "(defpackage :my-pkg (:use :cl))"))
           ;; Step 2: add in-package
           (nodes2 (parse-top-level-forms code)))
      (multiple-value-bind (target2 root2)
          (resolve-target nodes2 code)
        (let ((code (transform-insert-after code root2 target2
                                            :new-code "(in-package :my-pkg)")))
          ;; Step 3: add a function
          (let ((nodes3 (parse-top-level-forms code)))
            (multiple-value-bind (target3 root3)
                (resolve-target nodes3 code :target "(in-package :my-pkg)")
              (let ((final (transform-insert-after code root3 target3
                                                   :new-code "(defun hello () (format t \"Hello!\"))")))
                (ok (search "defpackage" final))
                (ok (search "in-package" final))
                (ok (search "defun hello" final))))))))))


;;;; JSON AST tests

(deftest test-json-ast-basic ()
  (testing "JSON AST contains expected fields"
    (let* ((text "(defun foo (x) (+ x 1))")
           (nodes (parse-top-level-forms text))
           (json (cst-to-json-ast nodes text)))
      (ok (search "\"type\"" json))
      (ok (search "\"list\"" json))
      (ok (search "\"head\"" json))
      (ok (search "\"defun\"" json))
      (ok (search "\"path\"" json))
      (ok (search "\"children\"" json)))))

(deftest test-json-ast-multiple-forms ()
  (testing "JSON AST handles multiple top-level forms"
    (let* ((text "(defun foo () 1) (defvar *x* 42)")
           (nodes (parse-top-level-forms text))
           (json (cst-to-json-ast nodes text)))
      (ok (search "defun" json))
      (ok (search "defvar" json)))))

(deftest test-json-ast-atom ()
  (testing "JSON AST represents atoms"
    (let* ((text "(+ 1 \"hello\")")
           (nodes (parse-top-level-forms text))
           (json (cst-to-json-ast nodes text)))
      (ok (search "\"number\"" json))
      (ok (search "\"string\"" json))
      (ok (search "\"symbol\"" json)))))
