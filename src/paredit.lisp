(uiop:define-package #:40ants-lisp-dev-mcp/paredit
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan-to-strings
                #:regex-replace-all)
  (:import-from #:40ants-lisp-dev-mcp/cst
                #:cst-node
                #:make-cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:cst-node-end-line
                #:parse-top-level-forms)
  (:export ;; Target resolution
           #:resolve-target
           ;; Transform functions
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
           ;; Read-only operations
           #:show-structure
           #:get-enclosing
           #:cst-to-json-ast
           ;; Helpers used by tools
           #:paredit-error
           #:node-source
           #:node-list-p
           #:find-parent))
(in-package #:40ants-lisp-dev-mcp/paredit)


;;;; Error handling

(define-condition paredit-error (simple-error)
  ()
  (:documentation "Signaled when a structural edit cannot be performed."))

(defun op-error (fmt &rest args)
  "Signal a PAREDIT-ERROR with a formatted message."
  (error 'paredit-error
         :format-control fmt
         :format-arguments args))


;;;; Node predicates and accessors

(defun node-list-p (node)
  "True if NODE is an :expr CST node whose value is a cons (i.e. a list form)."
  (and (typep node 'cst-node)
       (eq (cst-node-kind node) :expr)
       (consp (cst-node-value node))))

(defun node-atom-p (node)
  (and (typep node 'cst-node)
       (eq (cst-node-kind node) :expr)
       (atom (cst-node-value node))))

(defun node-source (text node)
  "Extract the source text for NODE from TEXT."
  (subseq text (cst-node-start node) (cst-node-end node)))

(defun expr-children (node)
  "Return only :expr CST-NODE children of NODE."
  (remove-if-not (lambda (c)
                   (and (typep c 'cst-node)
                        (eq (cst-node-kind c) :expr)))
                 (cst-node-children node)))

(defun node-head-name (node)
  "If NODE is a list whose CAR is a symbol, return its lowercase name."
  (when (node-list-p node)
    (let ((children (expr-children node)))
      (when (and children
                 (node-atom-p (first children))
                 (symbolp (cst-node-value (first children))))
        (string-downcase (symbol-name (cst-node-value (first children))))))))

(defun normalize-string (thing)
  (string-downcase (princ-to-string thing)))


;;;; Top-level form matching

(defun defmethod-candidates (form)
  "Generate candidate name strings for a defmethod form."
  (destructuring-bind (_head name &rest rest) form
    (declare (ignore _head))
    (let ((qualifiers '())
          (lambda-list nil))
      (dolist (part rest)
        (when (listp part)
          (setf lambda-list part)
          (return))
        (push part qualifiers))
      (let ((name-str (normalize-string name))
            (lambda-str (when lambda-list
                          (normalize-string (format nil "~S" lambda-list))))
            (qual-str (when qualifiers
                        (normalize-string
                         (format nil "~{~S~^ ~}" (nreverse qualifiers))))))
        (remove nil (list name-str
                         (when qual-str
                           (format nil "~A ~A" name-str qual-str))
                         (when lambda-str
                           (format nil "~A ~A" name-str lambda-str))
                         (when (and qual-str lambda-str)
                           (format nil "~A ~A ~A"
                                   name-str qual-str lambda-str))))))))

(defun definition-candidates (form form-type)
  "Generate candidate name strings for a definition form."
  (let ((name (second form)))
    (cond
      ((string= form-type "defmethod") (defmethod-candidates form))
      ((symbolp name) (list (normalize-string name)))
      (t (list (normalize-string name))))))

(defun find-top-level-form (nodes form-type form-name)
  "Find a top-level CST node matching FORM-TYPE and FORM-NAME.
   FORM-NAME can include [N] index suffix for disambiguation."
  (multiple-value-bind (base-name index)
      (let ((match (nth-value 1 (scan-to-strings
                                 "^(.+?)\\[(\\d+)\\]$" form-name))))
        (if match
            (values (aref match 0) (parse-integer (aref match 1)))
            (values form-name nil)))
    (let ((target (string-downcase base-name))
          (ft (string-downcase form-type))
          (matches nil))
      (loop for node in nodes
            when (and (typep node 'cst-node)
                      (eq (cst-node-kind node) :expr)
                      (consp (cst-node-value node)))
              do (let ((val (cst-node-value node)))
                   (when (and (string= (string-downcase
                                        (symbol-name (car val)))
                                       ft)
                              (some (lambda (c) (string= c target))
                                    (definition-candidates val ft)))
                     (push node matches))))
      (setf matches (nreverse matches))
      (cond
        ((null matches) nil)
        ((and index (< index (length matches)))
         (nth index matches))
        (index
         (op-error "Index [~D] out of range, ~D match~:P found"
                   index (length matches)))
        ((= (length matches) 1)
         (first matches))
        (t (op-error "~D matches for ~A ~A. Use [N] index to disambiguate."
                     (length matches) form-type form-name))))))


;;;; Target resolution

(defun whitespace-normalize (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (regex-replace-all "\\s+" text " ")))

(defun collect-all-nodes (root)
  "Depth-first collection of all :expr CST nodes under ROOT."
  (let ((result '()))
    (labels ((walk (node)
               (when (and (typep node 'cst-node)
                          (eq (cst-node-kind node) :expr))
                 (push node result)
                 (dolist (child (cst-node-children node))
                   (walk child)))))
      (walk root))
    (nreverse result)))

(defun resolve-by-path (root path)
  "Navigate into ROOT using PATH (list of child indices)."
  (let ((current root))
    (dolist (idx path current)
      (unless (node-list-p current)
        (op-error "Cannot navigate into atom at path index ~D" idx))
      (let ((children (expr-children current)))
        (when (>= idx (length children))
          (op-error "Path index ~D out of range (~D children)"
                    idx (length children)))
        (setf current (nth idx children))))))

(defun resolve-by-text (root target-text source-text &key line)
  "Find a descendant of ROOT whose source matches TARGET-TEXT."
  (let ((normalized (whitespace-normalize target-text))
        (nodes (collect-all-nodes root))
        (exact '())
        (prefix '()))
    (dolist (node nodes)
      (let* ((src (node-source source-text node))
             (nsrc (whitespace-normalize src)))
        (cond
          ((string= nsrc normalized) (push node exact))
          ((and (> (length nsrc) (length normalized))
                (string= nsrc normalized :end1 (length normalized)))
           (push node prefix)))))
    (let ((matches (or (nreverse exact) (nreverse prefix))))
      (cond
        ((null matches)
         (op-error "No matching node for target: ~A" target-text))
        ((= (length matches) 1)
         (first matches))
        (line
         (first (sort matches #'<
                      :key (lambda (n)
                             (abs (- (cst-node-start-line n) line))))))
        (t
         (op-error "~D matches for target. Add line hint or use path."
                   (length matches)))))))

(defun resolve-target (nodes text &key form-type form-name target path line)
  "Unified target resolution. Returns (values target-node root-node).
   ROOT-NODE is either the matched top-level form or a synthetic root
   wrapping all nodes."
  (let ((root (if (and form-type form-name)
                  (or (find-top-level-form nodes form-type form-name)
                      (op-error "Top-level form ~A ~A not found"
                                form-type form-name))
                  (if (= 1 (length nodes))
                      (first nodes)
                      (make-cst-node :kind :expr
                                     :value nil
                                     :children nodes
                                     :start 0
                                     :end (length text))))))
    (cond
      (path (values (resolve-by-path root path) root))
      (target (values (resolve-by-text root target text :line line) root))
      (t (values root root)))))


;;;; Tree navigation

(defun find-parent (root target)
  "Find the parent of TARGET within ROOT. NIL if TARGET is ROOT."
  (labels ((search-in (node)
             (dolist (child (cst-node-children node))
               (when (typep child 'cst-node)
                 (when (eq child target)
                   (return-from find-parent node))
                 (search-in child)))))
    (search-in root)
    nil))

(defun find-siblings (parent target)
  "Returns (values before-list target-node after-list)
   among PARENT's :expr children."
  (let ((children (expr-children parent))
        (before '())
        (found nil)
        (after '()))
    (dolist (child children)
      (cond (found (push child after))
            ((eq child target) (setf found child))
            (t (push child before))))
    (values (nreverse before) found (nreverse after))))


;;;; Transform functions
;;;; All transforms are pure: (text root target &key ...) -> new-text

(defun transform-wrap (text root target &key (count 1) wrapper head)
  "Wrap TARGET (and optionally COUNT-1 following siblings) in new delimiters."
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot wrap: target has no parent"))
    (multiple-value-bind (_bef _found aft) (find-siblings parent target)
      (declare (ignore _bef _found))
      (let* ((wrap-nodes (cons target
                               (subseq aft 0 (min (1- count) (length aft)))))
             (rstart (cst-node-start (first wrap-nodes)))
             (rend (cst-node-end (car (last wrap-nodes))))
             (region (subseq text rstart rend))
             (open (cond ((or (null wrapper) (string= wrapper "round")) "(")
                         ((string= wrapper "square") "[")
                         ((string= wrapper "curly") "{")
                         (t "(")))
             (close (cond ((or (null wrapper) (string= wrapper "round")) ")")
                          ((string= wrapper "square") "]")
                          ((string= wrapper "curly") "}")
                          (t ")")))
             (wrapped (if head
                          (format nil "~A~A ~A~A" open head region close)
                          (format nil "~A~A~A" open region close))))
        (concatenate 'string
                     (subseq text 0 rstart) wrapped (subseq text rend))))))

(defun transform-unwrap (text root target &key (keep "all"))
  "Remove list delimiters from TARGET, splicing children into parent.
   KEEP: \"all\" keeps all children, \"body\" drops the operator."
  (declare (ignore root))
  (unless (node-list-p target)
    (op-error "Cannot unwrap: target is not a list"))
  (let* ((children (expr-children target))
         (keep-children (if (string= keep "body")
                            (rest children)
                            children)))
    (when (null keep-children)
      (op-error "Cannot unwrap: no children to keep"))
    (let ((replacement
            (format nil "~{~A~^ ~}"
                    (mapcar (lambda (n) (node-source text n))
                            keep-children))))
      (concatenate 'string
                   (subseq text 0 (cst-node-start target))
                   replacement
                   (subseq text (cst-node-end target))))))

(defun transform-raise (text root target)
  "Replace parent with TARGET, effectively raising it one level."
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot raise: target has no parent"))
    (concatenate 'string
                 (subseq text 0 (cst-node-start parent))
                 (node-source text target)
                 (subseq text (cst-node-end parent)))))

(defun transform-slurp-forward (text root target &key (count 1))
  "Pull COUNT following sibling(s) into TARGET list."
  (unless (node-list-p target)
    (op-error "Cannot slurp: target is not a list"))
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot slurp: target has no parent"))
    (multiple-value-bind (_bef _found aft) (find-siblings parent target)
      (declare (ignore _bef _found))
      (when (null aft)
        (op-error "Cannot slurp forward: no sibling after target"))
      (let* ((slurp-nodes (subseq aft 0 (min count (length aft))))
             (last-slurped (car (last slurp-nodes)))
             (close-pos (1- (cst-node-end target)))
             (slurp-end (cst-node-end last-slurped))
             (slurped (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (subseq text (cst-node-end target)
                                           slurp-end)))
             (close-char (char text close-pos)))
        (concatenate 'string
                     (subseq text 0 close-pos)
                     " " slurped (string close-char)
                     (subseq text slurp-end))))))

(defun transform-slurp-backward (text root target &key (count 1))
  "Pull COUNT preceding sibling(s) into TARGET list."
  (unless (node-list-p target)
    (op-error "Cannot slurp: target is not a list"))
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot slurp: target has no parent"))
    (multiple-value-bind (bef _found _aft) (find-siblings parent target)
      (declare (ignore _found _aft))
      (when (null bef)
        (op-error "Cannot slurp backward: no sibling before target"))
      (let* ((slurp-nodes (last bef (min count (length bef))))
             (first-slurped (first slurp-nodes))
             (slurp-start (cst-node-start first-slurped))
             (open-pos (cst-node-start target))
             (open-char (char text open-pos))
             (slurped (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (subseq text slurp-start open-pos))))
        (concatenate 'string
                     (subseq text 0 slurp-start)
                     (string open-char) slurped " "
                     (subseq text (1+ open-pos)))))))

(defun transform-barf-forward (text root target &key (count 1))
  "Push last COUNT child(ren) out of TARGET as following sibling(s)."
  (declare (ignore root))
  (unless (node-list-p target)
    (op-error "Cannot barf: target is not a list"))
  (let* ((children (expr-children target))
         (n (length children)))
    (when (<= n count)
      (op-error "Cannot barf ~D children: list only has ~D" count n))
    (let* ((last-kept (nth (- n count 1) children))
           (first-barfed (nth (- n count) children))
           (close-pos (1- (cst-node-end target)))
           (close-char (char text close-pos))
           (barfed (string-trim
                    '(#\Space #\Tab #\Newline #\Return)
                    (subseq text (cst-node-start first-barfed)
                            close-pos))))
      (concatenate 'string
                   (subseq text 0 (cst-node-end last-kept))
                   (string close-char) " " barfed
                   (subseq text (1+ close-pos))))))

(defun transform-barf-backward (text root target &key (count 1))
  "Push first COUNT child(ren) out of TARGET as preceding sibling(s)."
  (declare (ignore root))
  (unless (node-list-p target)
    (op-error "Cannot barf: target is not a list"))
  (let* ((children (expr-children target))
         (n (length children)))
    (when (<= n count)
      (op-error "Cannot barf ~D children: list only has ~D" count n))
    (let* ((first-kept (nth count children))
           (open-pos (cst-node-start target))
           (open-char (char text open-pos))
           (barfed (string-trim
                    '(#\Space #\Tab #\Newline #\Return)
                    (subseq text (1+ open-pos)
                            (cst-node-start first-kept)))))
      (concatenate 'string
                   (subseq text 0 open-pos)
                   barfed " "
                   (string open-char)
                   (subseq text (cst-node-start first-kept))))))

(defun transform-kill (text root target &key (count 1))
  "Delete TARGET and optionally COUNT-1 following siblings."
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot kill: target has no parent"))
    (multiple-value-bind (bef _found aft) (find-siblings parent target)
      (declare (ignore _found))
      (let* ((kill-nodes (cons target
                               (subseq aft 0
                                       (min (1- count) (length aft)))))
             (kill-start (cst-node-start (first kill-nodes)))
             (kill-end (cst-node-end (car (last kill-nodes))))
             (remaining-aft (nthcdr (min (1- count) (length aft)) aft))
             (has-before (not (null bef)))
             (has-after (not (null remaining-aft))))
        (cond
          ((and has-before has-after)
           (let ((clean-start
                   (loop for i from (1- kill-start) downto 0
                         for ch = (char text i)
                         while (member ch '(#\Space #\Tab))
                         finally (return (1+ i)))))
             (concatenate 'string
                          (subseq text 0 clean-start)
                          (subseq text kill-end))))
          (has-before
           (let ((clean-start
                   (loop for i from (1- kill-start) downto 0
                         for ch = (char text i)
                         while (member ch '(#\Space #\Tab
                                            #\Newline #\Return))
                         finally (return (1+ i)))))
             (concatenate 'string
                          (subseq text 0 clean-start)
                          (subseq text kill-end))))
          (t
           (let* ((len (length text))
                  (clean-end
                    (loop for i from kill-end below len
                          for ch = (char text i)
                          while (member ch '(#\Space #\Tab
                                             #\Newline #\Return))
                          finally (return i))))
             (concatenate 'string
                          (subseq text 0 kill-start)
                          (subseq text clean-end)))))))))

(defun transform-transpose (text root target)
  "Swap TARGET with its next sibling."
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot transpose: target has no parent"))
    (multiple-value-bind (_bef _found aft) (find-siblings parent target)
      (declare (ignore _bef _found))
      (when (null aft)
        (op-error "Cannot transpose: no next sibling"))
      (let* ((next (first aft))
             (a-start (cst-node-start target))
             (a-end (cst-node-end target))
             (b-start (cst-node-start next))
             (b-end (cst-node-end next))
             (a-text (subseq text a-start a-end))
             (b-text (subseq text b-start b-end))
             (between (subseq text a-end b-start)))
        (concatenate 'string
                     (subseq text 0 a-start)
                     b-text between a-text
                     (subseq text b-end))))))

(defun transform-split (text root target &key clone-head)
  "Split parent list at TARGET. TARGET becomes first element of second list.
   CLONE-HEAD: if true, copy operator to both halves."
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot split: target has no parent"))
    (unless (node-list-p parent)
      (op-error "Cannot split: parent is not a list"))
    (let* ((p-start (cst-node-start parent))
           (p-end (cst-node-end parent))
           (open-char (char text p-start))
           (close-char (char text (1- p-end))))
      (multiple-value-bind (before _found after) (find-siblings parent target)
        (declare (ignore _found))
        (let* ((left before)
               (right (cons target after))
               (children (expr-children parent))
               (head-text (when (and clone-head left)
                            (node-source text (first children))))
               (left-str (format nil "~{~A~^ ~}"
                                 (mapcar (lambda (n) (node-source text n))
                                         left)))
               (right-parts
                 (if (and clone-head head-text)
                     (cons head-text
                           (mapcar (lambda (n) (node-source text n))
                                   right))
                     (mapcar (lambda (n) (node-source text n)) right)))
               (right-str (format nil "~{~A~^ ~}" right-parts)))
          (concatenate 'string
                       (subseq text 0 p-start)
                       (string open-char) left-str (string close-char)
                       " "
                       (string open-char) right-str (string close-char)
                       (subseq text p-end)))))))

(defun transform-join (text root target &key drop-head)
  "Join TARGET list with its next sibling list.
   DROP-HEAD: if true, discard the second list's operator."
  (unless (node-list-p target)
    (op-error "Cannot join: target is not a list"))
  (let ((parent (find-parent root target)))
    (unless parent (op-error "Cannot join: target has no parent"))
    (multiple-value-bind (_bef _found aft) (find-siblings parent target)
      (declare (ignore _bef _found))
      (when (null aft)
        (op-error "Cannot join: no next sibling"))
      (let ((next (first aft)))
        (unless (node-list-p next)
          (op-error "Cannot join: next sibling is not a list"))
        (let* ((close-pos (1- (cst-node-end target)))
               (close-char (char text close-pos))
               (next-children (expr-children next))
               (merge (if drop-head (rest next-children) next-children))
               (merge-text (format nil "~{~A~^ ~}"
                                   (mapcar (lambda (n) (node-source text n))
                                           merge))))
          (concatenate 'string
                       (subseq text 0 close-pos)
                       (if merge " " "")
                       merge-text
                       (string close-char)
                       (subseq text (cst-node-end next))))))))

(defun transform-replace (text root target &key new-code)
  "Replace TARGET node with NEW-CODE."
  (declare (ignore root))
  (unless new-code
    (op-error "Cannot replace: no new-code provided"))
  (concatenate 'string
               (subseq text 0 (cst-node-start target))
               new-code
               (subseq text (cst-node-end target))))

(defun transform-insert-before (text root target &key new-code)
  "Insert NEW-CODE as a sibling before TARGET.
   When TARGET is the same as ROOT (no target specified), prepend to the text."
  (declare (ignore root))
  (unless new-code
    (op-error "Cannot insert: no new-code provided"))
  (if (zerop (length text))
      new-code
      (let ((pos (cst-node-start target)))
        (if (zerop pos)
            (concatenate 'string new-code (string #\Newline) (string #\Newline) text)
            (concatenate 'string
                         (subseq text 0 pos)
                         new-code " "
                         (subseq text pos))))))

(defun transform-insert-after (text root target &key new-code)
  "Insert NEW-CODE as a sibling after TARGET.
   When TARGET is the same as ROOT (no target specified), append to the text."
  (declare (ignore root))
  (unless new-code
    (op-error "Cannot insert: no new-code provided"))
  (if (zerop (length text))
      new-code
      (let ((pos (cst-node-end target)))
        (if (= pos (length text))
            (concatenate 'string text (string #\Newline) (string #\Newline) new-code)
            (concatenate 'string
                         (subseq text 0 pos)
                         " " new-code
                         (subseq text pos))))))


;;;; Read-only operations

(defun node-kind-string (node)
  (cond
    ((eq (cst-node-kind node) :skipped) "comment")
    ((node-list-p node) "list")
    ((symbolp (cst-node-value node)) "symbol")
    ((stringp (cst-node-value node)) "string")
    ((numberp (cst-node-value node)) "number")
    (t "atom")))

(defun truncate-text (text max-len)
  (if (<= (length text) max-len)
      text
      (concatenate 'string (subseq text 0 (- max-len 3)) "...")))

(defun show-structure (root text &key (depth 4) (max-text 60))
  "Build a human-readable tree representation of ROOT's structure."
  (with-output-to-string (out)
    (labels ((print-node (node path d indent)
               (when (and (typep node 'cst-node)
                          (eq (cst-node-kind node) :expr))
                 (format out "~A[~{~D~^.~}] ~A L~D: ~A~%"
                         indent path
                         (node-kind-string node)
                         (cst-node-start-line node)
                         (truncate-text (node-source text node) max-text))
                 (when (and (node-list-p node) (< d depth))
                   (loop for child in (expr-children node)
                         for i from 0
                         do (print-node child
                                        (append path (list i))
                                        (1+ d)
                                        (concatenate 'string indent "  ")))))))
      (if (node-list-p root)
          (loop for child in (expr-children root)
                for i from 0
                do (print-node child (list i) 1 ""))
          (print-node root '() 0 "")))))

(defun get-enclosing (root target text &key (levels 1))
  "Return info about the enclosing form of TARGET as a formatted string."
  (let ((current target)
        (enclosing nil))
    (dotimes (_ levels)
      (setf enclosing (find-parent root current))
      (unless enclosing (return))
      (setf current enclosing))
    (unless enclosing
      (return-from get-enclosing "No enclosing form at requested level"))
    (let ((children (expr-children enclosing))
          (child-idx nil))
      (loop for ch in children for i from 0
            when (eq ch (if (= levels 1) target current))
              do (setf child-idx i) (return))
      (format nil "head: ~A~%kind: ~A~%child_index: ~A~%siblings: ~D~%line: ~D~%text: ~A"
              (or (node-head-name enclosing) "nil")
              (node-kind-string enclosing)
              (or child-idx -1)
              (length children)
              (cst-node-start-line enclosing)
              (truncate-text (node-source text enclosing) 200)))))


(defun cst-to-json-ast (nodes text &key (depth 6) (max-text 120))
  "Convert CST nodes to a JSON string representing the semantic AST.
   Each node is an object with: type, path, line, start, end, text,
   and optionally head (for lists) and children."
  (labels ((node-to-ht (node path d)
             (when (and (typep node 'cst-node)
                        (eq (cst-node-kind node) :expr))
               (let ((ht (make-hash-table :test 'equal)))
                 (setf (gethash "type" ht) (node-kind-string node)
                       (gethash "path" ht) (coerce path 'vector)
                       (gethash "line" ht) (cst-node-start-line node)
                       (gethash "start" ht) (cst-node-start node)
                       (gethash "end" ht) (cst-node-end node)
                       (gethash "text" ht) (truncate-text
                                            (node-source text node)
                                            max-text))
                 (when (node-list-p node)
                   (let ((head (node-head-name node)))
                     (when head
                       (setf (gethash "head" ht) head)))
                   (when (< d depth)
                     (let ((ch (loop for child in (expr-children node)
                                     for i from 0
                                     for sub = (node-to-ht child
                                                           (append path (list i))
                                                           (1+ d))
                                     when sub collect sub)))
                       (when ch
                         (setf (gethash "children" ht)
                               (coerce ch 'vector))))))
                 ht))))
    (let ((top-nodes (loop for node in nodes
                           for i from 0
                           for ht = (node-to-ht node (list i) 1)
                           when ht collect ht)))
      (with-output-to-string (out)
        (yason:encode (coerce top-nodes 'vector) out)))))
