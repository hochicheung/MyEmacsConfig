;;; Config Locations
(defvar my/config (expand-file-name (concat user-emacs-directory "config.org")))
(defvar my/config-exported (expand-file-name (concat user-emacs-directory "config.el")))
;;(defvar my/config-compiled (expand-file-name (concat user-emacs-directory "config.elc")))
;;(defvar my/config-stable (expand-file-name (concat user-emacs-directory "stable.elc")))

;;(setq debug-on-error t)
;;(setq debug-ignored-errors '(void-variable
;;void-function
;;wrong-type-argument
;;end-of-file))
;;(setq debug-on-signal t)
;; Is config.org updated?

;;(defun my/is-file-updated(file1 file2)
;;(time-less-p (nth 5 (file-attributes file1)) (nth 5 (file-attributes file2))))

;;; Export config.org --> config.el
;;(defun my/export-config()
;;(condition-case nil
;;(progn
;;(require 'ob-tangle)
;;(org-babel-tangle-file my/config my/config-exported "emacs-lisp")
;;(message "Exported config.org"))
;;((error)(message "Config.org was NOT exported"))))

;;; Compile config.el --> config.elc
;;(defun my/compile-exported()
;;(condition-case nil
;;(progn
;;(setq byte-compile-warnings '(not
;;nresolved
;;free-vars
;;unresolved
;;callargs
;;redefine
;;noruntime
;;cl-functions
;;interactive-only))
;;(byte-compile-file my/config-exported)
;;(message "Compiled config.el"))
;;((error)(message "Config.el was NOT compiled"))))

;;; Load config.elc
;;(defun my/load-compiled()
;;(condition-case nil
;;(progn
;;(load-file my/config-compiled)
;;(message "Loaded config.elc")
;;(delete-file my/config-stable)
;;(rename-file my/config-compiled my/config-stable)
;;(message "Marked stable.elc"))
;;((error)(progn
;;(load-file my/config-stable)
;;(message "Could NOT load config.elc, config.org implementation error")
;;(message "Loaded stable.elc")))))

;;; Main()
;;(if (or (my/is-file-updated my/config-exported my/config) (not (file-exists-p my/config-exported)))
;;(condition-case nil
;;(progn (my/export-config)
;;(condition-case nil
;;(progn (my/compile-exported)
;;(my/load-compiled))
;;((error) (progn (my/load-compiled)
;;(message "Config.el was not compiled, Loading config.elc")))))
;;((error) (progn (my/load-compiled)
;;(message "Config.org was NOT exported"))))
;;(progn (load-file my/config-stable)
;;(message "Loaded stable.elc")))
;;(if (file-exists-p my/config-compiled)
;;(message "Config.org has errors"))

;;; Temporary code
(defun my/export-config()
  (condition-case nil
	  (progn
		(require 'ob-tangle)
		(org-babel-tangle-file my/config my/config-exported "emacs-lisp"))))
(my/export-config)
(load-file my/config-exported)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
	("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(hl-todo-keyword-faces
   (quote
	(("TODO" . "#dc752f")
	 ("NEXT" . "#dc752f")
	 ("THEM" . "#2d9574")
	 ("PROG" . "#4f97d7")
	 ("OKAY" . "#4f97d7")
	 ("DONT" . "#f2241f")
	 ("FAIL" . "#f2241f")
	 ("DONE" . "#86dc2f")
	 ("NOTE" . "#b1951d")
	 ("KLUDGE" . "#b1951d")
	 ("HACK" . "#b1951d")
	 ("TEMP" . "#b1951d")
	 ("FIXME" . "#dc752f")
	 ("XXX+" . "#dc752f")
	 ("\\?\\?\\?+" . "#dc752f"))))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
