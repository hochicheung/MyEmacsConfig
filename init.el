(setq debug-on-error t)

;; Config Locations
(defvar my/config (expand-file-name (concat user-emacs-directory "config.org")))
(defvar my/config-exported (expand-file-name (concat user-emacs-directory "config.el")))
(defvar my/config-compiled (expand-file-name (concat user-emacs-directory "config.elc")))

;; Latest modified (is file1 latestmodified)
(defun my/is-file-latestmodified (file1 file2)
  (time-less-p (nth 5 (file-attributes file2)) (nth 5 (file-attributes file1))))

;; Export .org --> .el
(defun my/export-config()
  (require 'ob-tangle)
  (ignore-errors
    (org-babel-tangle-file my/config my/config-exported "emacs-lisp")))

;; Compile .el --> .elc
(defun my/compile-exported()
  (setq byte-compile-warnings '(not
				nresolved
				free-vars
				unresolved
				callargs
				redefine
				noruntime
				cl-functions
				interactive-only))
  (ignore-errors
    (byte-compile-file my/config-exported)))

;; Load .elc
(defun my/loadc()
  (ignore-errors
    (load-file my/config-compiled)))
(defun my/load-compiled()
  (cond
   ((my/loadc) (message "Config.elc Loaded"))
   ((not my/loadc) (message "Config.elc was Not-Loaded"))))

;; Main
;; If (if config isLatestmodifed versus exported) OR (exported doesn't exist)
(if (or (my/is-file-latestmodified my/config my/config-exported) (not (file-exists-p my/config-exported)))
    (cond
     ((my/export-config) (cond
			  ((my/compile-exported) (my/load-compiled))
			  ((not my/compile-exported) (progn (message "Config.el was not compiled, Loading config.elc")
							     (my/load-compiled)))))
     ((not my/export-config) (progn (message "Config.org was NOT exported")
				    (my/load-compiled))))
    (my/load-compiled))

