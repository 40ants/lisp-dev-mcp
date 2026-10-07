(uiop:define-package #:40ants-lisp-dev-mcp-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing
                #:signals)
  (:import-from #:40ants-lisp-dev-mcp/core
                #:start-server
                #:choose-port
                #:get-port-from-assistant-config
                #:update-port-in-config
                #:*codex-config-pathname*
                #:read-config)
  (:import-from #:cl-toml)
  (:import-from #:find-port)
  (:import-from #:40ants-mcp/server/definition)
  (:import-from #:40ants-lisp-dev-mcp/main
                #:parse-agents)
  (:import-from #:alexandria
                #:write-string-into-file))
(in-package #:40ants-lisp-dev-mcp-tests/core)


(defmacro with-configs ((opencode codex) &body body)
  (let ((directory (gensym "DIRECTORY")))
    `(let* ((,directory (merge-pathnames (format nil "lisp-dev-mcp-~A/" (gensym))
                                       (uiop:temporary-directory)))
            (,opencode (merge-pathnames "opencode.json" ,directory))
            (,codex (merge-pathnames ".codex/config.toml" ,directory)))
       (declare (ignorable ,opencode ,codex))
       (unwind-protect
            (progn (ensure-directories-exist ,codex) (locally ,@body))
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore)))))

(defmacro with-function ((name replacement) &body body)
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (symbol-function ',name)))
       (unwind-protect
            (progn (setf (symbol-function ',name) ,replacement) ,@body)
         (setf (symbol-function ',name) ,original)))))

(defmacro with-fake-server (&body body)
  `(with-function (40ants-mcp/server/definition:start-server
                  (lambda (&rest args) args))
     (with-function (find-port:port-open-p (lambda (port) (declare (ignore port)) t))
       ,@body)))

(defun put-config (path content)
  (ensure-directories-exist path)
  (write-string-into-file content path :if-exists :supersede))

(defun codex-server (path)
  (gethash "lisp-dev-mcp" (gethash "mcp_servers" (cl-toml:parse-file path))))

(deftest codex-config ()
  (with-configs (opencode codex)
    (testing "An existing file without MCP keeps its features"
      (put-config codex (format nil "[features]~%hooks = true~%"))
      (update-port-in-config 40001 :agent :codex :config codex)
      (let ((data (cl-toml:parse-file codex))
            (server (codex-server codex)))
        (ok (eq (gethash "hooks" (gethash "features" data)) 'cl-toml:true))
        (ok (equal (gethash "url" server) "http://localhost:40001/mcp"))
        (ok (eq (gethash "enabled" server) 'cl-toml:true))
        (ok (= (gethash "startup_timeout_sec" server) 10))
        (ok (= (gethash "tool_timeout_sec" server) 60))
        (ok (= (get-port-from-assistant-config :agent :codex :config codex) 40001))))
    (testing "Existing values and other servers survive"
      (put-config codex
                  (format nil "[mcp_servers.other]~%url = ~S~%[mcp_servers.lisp-dev-mcp]~%url = ~S~%enabled = false~%startup_timeout_sec = 3~%tool_timeout_sec = 120~%extra = ~S~%"
                          "http://example.com/mcp" "http://localhost:1/mcp" "keep"))
      (update-port-in-config 40002 :agent :codex :config codex)
      (let ((server (codex-server codex)))
        (ok (eq (gethash "enabled" server) 'cl-toml:false))
        (ok (= (gethash "startup_timeout_sec" server) 3))
        (ok (= (gethash "tool_timeout_sec" server) 120))
        (ok (equal (gethash "extra" server) "keep"))
        (ok (equal (gethash "url" server) "http://localhost:40002/mcp")))
      (ok (equal (gethash "url" (gethash "other"
                                       (gethash "mcp_servers" (cl-toml:parse-file codex))))
                 "http://example.com/mcp")))
    (testing "An empty file and a missing directory are supported"
      (put-config codex "")
      (update-port-in-config 40001 :agent :codex :config codex)
      (ok (codex-server codex))
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname codex) :validate t)
      (update-port-in-config 40001 :agent :codex :config codex)
      (ok (= (hash-table-count (cl-toml:parse-file codex)) 1)))
    (testing "The Codex pathname can be rebound"
      (let ((*codex-config-pathname* codex))
        (update-port-in-config 40002 :agent :codex)
        (ok (= (get-port-from-assistant-config :agent :codex) 40002))))))

(deftest opencode-config ()
  (with-configs (opencode codex)
    (update-port-in-config 40001 :config opencode)
    (ok (= (get-port-from-assistant-config :config opencode) 40001))
    (ok (equal (gethash "$schema" (read-config opencode))
               "https://opencode.ai/config.json"))
    (put-config opencode "{\"theme\":\"dark\",\"enabled\":false,\"items\":[],\"nothing\":null}")
    (update-port-in-config 40002 :config opencode)
    (let ((data (read-config opencode)))
      (ok (equal (gethash "theme" data) "dark"))
      (ok (eq (gethash "enabled" data) 'yason:false))
      (ok (equalp (gethash "items" data) #()))
      (ok (eq (gethash "nothing" data) :null)))))

(deftest agents-and-start-server ()
  (with-configs (opencode codex)
    (with-fake-server
      (testing "The default updates only OpenCode"
        (start-server :port 40001 :update-config t :in-thread nil
                      :opencode-config opencode :codex-config codex)
        (ok (probe-file opencode))
        (ok (not (probe-file codex))))
      (testing "A reused port still creates the Codex config"
        (start-server :port :auto :update-config t :in-thread nil
                      :agents '(:opencode :codex :codex)
                      :opencode-config opencode :codex-config codex)
        (ok (= (get-port-from-assistant-config :config opencode) 40001))
        (ok (= (get-port-from-assistant-config :agent :codex :config codex) 40001)))
      (testing "Selecting only Codex leaves OpenCode untouched"
        (let ((before (uiop:read-file-string opencode)))
          (start-server :port 40002 :update-config t :in-thread nil :agents '(:codex)
                        :opencode-config opencode :codex-config codex)
          (ok (equal before (uiop:read-file-string opencode)))
          (ok (= (get-port-from-assistant-config :agent :codex :config codex) 40002))))
      (testing "An empty list and disabled updates do not write files"
        (delete-file opencode)
        (delete-file codex)
        (start-server :port 40001 :update-config t :in-thread nil :agents nil
                      :opencode-config opencode :codex-config codex)
        (start-server :port 40001 :in-thread nil :agents '(:opencode :codex)
                      :opencode-config opencode :codex-config codex)
        (start-server :update-config t :in-thread nil :agents '(:opencode :codex)
                      :opencode-config opencode :codex-config codex)
        (ok (not (probe-file opencode)))
        (ok (not (probe-file codex))))
      (testing "Unknown agents are rejected before writes"
        (ok (signals (start-server :port 40001 :update-config t :in-thread nil
                                  :agents '(:opencode :unknown)
                                  :opencode-config opencode :codex-config codex)))
        (ok (signals (start-server :agents '(:opencode "codex"))))
        (ok (signals (start-server :agents :codex)))
        (ok (not (probe-file opencode)))))))

(deftest choose-agent-port ()
  (with-configs (opencode codex)
    (update-port-in-config 40001 :config opencode)
    (update-port-in-config 40002 :agent :codex :config codex)
    (with-function (find-port:port-open-p (lambda (port) (member port '(40001 40002))))
      (ok (= (choose-port :auto :agents '(:opencode :codex)
                         :config opencode :codex-config codex) 40001))
      (ok (= (choose-port "auto" :agents '(:codex :opencode)
                         :config opencode :codex-config codex) 40002))
      (ok (nth-value 1 (choose-port :auto :agents '(:opencode :codex)
                                   :config opencode :codex-config codex)))
      (ok (not (nth-value 1 (choose-port :auto :config opencode)))))
    (with-function (find-port:port-open-p (lambda (port) (= port 40002)))
      (ok (= (choose-port :auto :agents '(:opencode :codex)
                         :config opencode :codex-config codex) 40002))
      (ok (signals (choose-port 40001 :config opencode))))
    (with-function (find-port:port-open-p (lambda (port) (declare (ignore port)) nil))
      (with-function (find-port:find-port (lambda (&rest args) (declare (ignore args)) 40003))
        (ok (= (choose-port :auto :agents '(:opencode :codex)
                           :config opencode :codex-config codex) 40003))
        (put-config opencode "invalid JSON")
        (put-config codex "invalid TOML")
        (ok (= (choose-port :auto :agents nil
                           :config opencode :codex-config codex) 40003))))))

(deftest invalid-config ()
  (with-configs (opencode codex)
    (with-fake-server
      (dolist (content '("[broken" "mcp_servers = false"
                         "[mcp_servers]~%lisp-dev-mcp = 42~%"))
        (let ((content (format nil content)))
          (put-config codex content)
          (ok (signals (update-port-in-config 40001 :agent :codex :config codex)))
          (ok (equal (uiop:read-file-string codex) content))
          (ok (signals (start-server :port 40001 :update-config t :in-thread nil
                                    :agents '(:opencode :codex)
                                    :opencode-config opencode :codex-config codex)))
          (ok (not (probe-file opencode))))))))

(deftest cli-agents ()
  (ok (equal (parse-agents nil) '(:opencode)))
  (ok (equal (parse-agents " Codex , OPENCODE ") '(:codex :opencode)))
  (ok (equal (parse-agents "codex,codex") '(:codex)))
  (dolist (value '("" "other" "codex," ",opencode" "codex,,opencode"))
    (ok (signals (parse-agents value)))))

(deftest serialization-failure ()
  (with-configs (opencode codex)
    (put-config opencode "{\"theme\":\"dark\"}")
    (put-config codex (format nil "[features]~%hooks = true~%"))
    (let ((before-opencode (uiop:read-file-string opencode))
          (before-codex (uiop:read-file-string codex)))
      (with-fake-server
        (with-function (cl-toml:encode
                        (lambda (&rest args)
                          (error "Unable to serialize ~S." args)))
          (ok (signals (start-server :port 40001 :update-config t :in-thread nil
                                    :agents '(:opencode :codex)
                                    :opencode-config opencode :codex-config codex)))))
      (ok (equal before-opencode (uiop:read-file-string opencode)))
      (ok (equal before-codex (uiop:read-file-string codex))))))
