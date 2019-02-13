;; Emacs Init-file

;; Config Locations
(defvar my/config (expand-file-name (concat user-emacs-directory "config.org")))
(defvar my/config-exported (expand-file-name (concat user-emacs-directory "config.el")))
(defvar my/config-compiled (expand-file-name (concat user-emacs-directory "config.elc")))
(defvar my/config-stable (expand-file-name (concat user-emacs-directory "stable.elc")))

;; Is config.org updated?
(defun my/is-file-updated(file1 file2)
		(time-less-p (nth 5 (file-attributes file2)) (nth 5 (file-attributes file1))))

;; Export config.org --> config.el
(defun my/export-config()
  (condition-case nil
      (progn
				(require 'ob-tangle)
				(org-babel-tangle-file my/config my/config-exported "emacs-lisp")
				(message "Exported config.org"))
    ((error)(message "Config.org was NOT exported"))))

;; Compile config.el --> config.elc
(defun my/compile-exported()
  (condition-case nil
      (progn
				(setq byte-compile-warnings '(not
																			nresolved
																			free-vars
																			unresolved
																			callargs
																			redefine
																			noruntime
																			cl-functions
																			interactive-only))
				(byte-compile-file my/config-exported)
				(message "Compiled config.el"))
		((error)(message "Config.el was NOT compiled"))))

;; Load config.elc
(defun my/load-compiled()
  (condition-case nil
      (progn
				(load-file my/config-compiled)
				(message "Loaded config.elc")
				(delete-file my/config-stable)
				(rename-file my/config-compiled my/config-stable)
				(message "Marked stable.elc"))
    ((error)(progn
							(load-file my/config-stable)
							(message "Loaded stable.elc")))))

;; Main()
(if (or (my/is-file-updated my/config-exported my/config) (not (file-exists-p my/config-exported)))
    (condition-case nil
				(progn (my/export-config)
							 (condition-case nil
									 (progn (my/compile-exported)
													(my/load-compiled))
								 ((error) (progn (my/load-compiled)
																 (message "Config.el was not compiled, Loading config.elc")))))
      ((error) (progn (my/load-compiled)
											(message "Config.org was NOT exported"))))
  (load-file my/config-stable))
