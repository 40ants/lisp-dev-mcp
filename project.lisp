;; We need this structrure as a workaround
;; because by default when you are installing project from github
;; like ros install 40ants/lisp-dev-mcp
;; roswell will search ASDF system lisp-dev-mcp, but we have
;; ASDF system 40ants-lisp-dev-mcp.asd, not matching to a repository name.
;; Issue: https://github.com/roswell/roswell/issues/614
(nil 
 (
  (nil
   (
    nil
    (("asd" "40ants-lisp-dev-mcp"))))))
