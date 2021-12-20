;;; -*- lexical-binding: t; -*-

;;; Startup

;;;; Device config
(defun my/load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

;; If a device config is not made, load the default one
(if (not (my/load-if-exists (concat user-emacs-directory "device.el")))
    (load-file (concat user-emacs-directory "device-template.el")))

;;; Emacs Settings

;;;; Keyboard Layout
(shell-command
 "setxkbmap -layout us -variant altgr-intl -option caps:escape")

;;;; Emacs UI
(menu-bar-mode -1)
(display-time-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode 1)
(global-visual-line-mode t)
(fringe-mode 0)
(display-battery-mode t)

;;;; Emacs Misc
(setq-default tab-width 2)
(mouse-wheel-mode -1)
(setq show-paren-delay 0)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
			kept-new-versions 4
			kept-old-versions 2
			version-control t)

(setq enable-recursive-minibuffers t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Straight
(eval-and-compile (defvar bootstrap-version)
(let ((bootstrap-file
			 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
			(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
				(url-retrieve-synchronously
				 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage)))

;;;; My/directory-file-p-nil-create functions

;;;;; My/directory-p-nil-create
(defun my/directory-p-nil-create (directory)
	(when (not (file-exists-p directory))
		(message "Creating directory %s" directory)
		(make-directory directory t)))

;;;;; My/file-exists-p-nil-create
(defun my/file-exists-p-nil-create (file &optional content)
	(when (not (file-exists-p file))
		(if content
				(progn (message "Creating file %s" file)
							 (write-region content nil file))
			(progn (message "Creating file %s" file)
						 (write-region "" nil file)))))

;;;; Prefered Webbrowser
(setq browse-url-browser-function 'browse-url-generic
			browse-url-generic-program "qutebrowser")

;;;; Battery
(require 'battery)

;;;; Line Numbers
;;(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; NixOS
;; Load emacs packages from nixOS directory
(setq load-path (append load-path (file-expand-wildcards (expand-file-name "~/.nix-profile/share/emacs/site-lisp/elpa/*"))))

;;;; Initial-buffer
(setq initial-buffer-choice t)

;;;; Mark-ring
(setq mark-ring-max 8)
(setq global-mark-ring-max 8)
(global-set-key (kbd "C-x C-2") 'pop-global-mark)

;;;; Split-threshold
(setq split-height-threshold nil)
(setq split-width-threshold 100)

;;; Essentials

;;;; Common-lisp
(require 'cl-lib)

;;;; Evil
(eval-and-compile
(straight-use-package 'evil)
(require 'evil))
(add-to-list 'load-path (concat user-emacs-directory "straight/build/undo-tree"))
(evil-mode)
(setq evil-emacs-state-modes nil
			evil-insert-state-modes nil
			evil-motion-state-modes nil)

(define-key evil-normal-state-map (kbd "C-u") (lambda ()
																								(interactive)
																								(evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-d") (lambda ()
																								(interactive)
																								(evil-scroll-down nil)))

(defun my-evil-record-macro ()
  (interactive)
  (if buffer-read-only
      (quit-window)
    (call-interactively 'evil-record-macro)))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "q") 'my-evil-record-macro))

(evil-set-undo-system 'undo-tree)

;;;;; Window
(evil-define-key 'normal 'evil-normal-state-map (kbd "s-h") 'previous-buffer)
(evil-define-key 'normal 'evil-normal-state-map (kbd "s-l") 'next-buffer)
(evil-define-key 'normal 'evil-normal-state-map (kbd "s-x") 'kill-this-buffer)

;;;;; Mouse-clicks
(dolist (mouseclicks-kill '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
														[mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
														[mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
														[mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
														[mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key mouseclicks-kill))
(define-key evil-motion-state-map [down-mouse-1] nil)
(define-key evil-normal-state-map [down-mouse-1] nil)

;;;; Org-mode
;;(load-library "org-autoloads")
(eval-and-compile (straight-use-package '(org :local-repo nil)))

(setq org-hide-emphasis-markers t)
(setq org-src-window-setup 'current-window)
(setq org-image-actual-width nil)
(setq org-log-done 'time)

;; persistent clock
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

(define-key org-mode-map (kbd "C-c l") 'org-store-link)

;;;;; Structured Templates
;; Templates are defined in ~org-structure-template-alist~
(require 'org-tempo)

;;;;; Timestamps
(setq time-stamp-active t     ; enable time-stamps
			time-stamp-line-limit 15 ;check first 15 lines for Time-stamp: <> or Time-stamp: " "
			time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S %Z (%u)") ; date format
(add-hook 'write-file-functions 'time-stamp) ; update time stamp when saving

;;;;; Org-agenda
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-c") 'org-capture)

(my/directory-p-nil-create "~/Org/agenda")
(my/file-exists-p-nil-create "~/Org/agenda/inbox.org" "")
(my/file-exists-p-nil-create "~/Org/agenda/projects.org" "")
(my/file-exists-p-nil-create "~/Org/agenda/tickler.org" "")
(my/file-exists-p-nil-create "~/Org/agenda/someday.org" "")

(setq org-agenda-files '("~/Org/agenda/projects.org"
												 "~/Org/agenda/inbox.org"
												 "~/Org/agenda/tickler.org"))

(setq org-agenda-window-setup 'only-window)
(setq org-agenda-restore-windows-after-quit t)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
															 (file "~/Org/agenda/inbox.org")
															 "* TODO%i%? \n %U")
															("T" "Tickler" entry
															 (file "~/Org/agenda/tickler.org")
															 "* %i%? \n %U \n %t")))

(setq org-refile-use-outline-path 'file)

(setq org-refile-targets '(("~/Org/agenda/projects.org" :level . 1)
													 ("~/Org/agenda/someday.org" :level . 1)
													 ("~/Org/agenda/tickler.org" :level . 1)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(add-hook 'org-agenda-finalize-hook (lambda() (setq org-agenda-block-separator (make-string (- (window-total-width) 1) ?—))))

(setq org-agenda-breadcrumbs-separator ":\n\t"
			org-agenda-start-on-weekday nil
			org-agenda-span 14
			org-agenda-start-day "-2d"
			org-agenda-show-all-dates nil
			org-agenda-use-time-grid nil)

(setq org-agenda-custom-commands 
      '(("o" "At office"
				 ((tags-todo "@office"((org-agenda-files '("~/Org/agenda/inbox.org"))
															 (org-agenda-prefix-format '((tags . "\n %b")))
															 (org-agenda-overriding-header "@office inbox")
															 (org-agenda-skip-function #'my/org-agenda-skip-nth-siblings-with-parents)))
					(tags-todo "@office"((org-agenda-files '("~/Org/agenda/projects.org"))
															 (org-agenda-prefix-format '((tags . "\n %b")))
															 (org-agenda-overriding-header "@office projects")
															 (org-agenda-skip-function #'my/org-agenda-skip-nth-siblings-with-parents)))))
				("h" "At home"
				 ((tags-todo "@home"((org-agenda-files '("~/Org/agenda/inbox.org"))
														 (org-agenda-prefix-format '((tags . "\n %b")))
														 (org-agenda-overriding-header "@home inbox")
														 (org-agenda-skip-function #'my/org-agenda-skip-nth-siblings-with-parents)))
					(tags-todo "@home"((org-agenda-files '("~/Org/agenda/projects.org"))
														 (org-agenda-prefix-format '((tags . "\n %b")))
														 (org-agenda-overriding-header "@home projects")
														 (org-agenda-skip-function #'my/org-agenda-skip-nth-siblings-with-parents)))))
				("i" "Inbox" todo ""
				 ((org-agenda-files '("~/Org/agenda/inbox.org"))
					(org-agenda-overriding-header "INBOX")))
				("p" "All Projects" todo ""
				 ((org-agenda-files '("~/Org/agenda/projects.org"))
					(org-agenda-overriding-header "All Projects")
					(org-agenda-prefix-format '((todo . "\n %b")))
					(org-agenda-skip-function #'my/org-agenda-skip-nth-siblings-with-parents)))
				("w" "Weekly Review"
				 ((agenda "" ((org-agenda-prefix-format '((agenda . "\t%t ")))))
					(todo "" ((org-agenda-files '("~/Org/agenda/inbox.org"))
										(org-agenda-prefix-format '((todo . "\t")))
										(org-agenda-overriding-header "INBOX")))
					(todo "" ((org-agenda-files '("~/Org/agenda/projects.org"))
										(org-agenda-prefix-format '((todo . " %b")))
										(org-agenda-overriding-header "PROJECTS")
										(org-agenda-skip-function #'my/org-agenda-skip-nth-siblings-with-parents)))
					(todo "" ((org-agenda-files '("~/Org/agenda/someday.org"))
										(org-agenda-prefix-format '((todo . "\t")))
										(org-agenda-overriding-header "SOMEDAY")))))))

(defun my/org-agenda-skip-nth-siblings-with-parents ()
	(let (skip-entry)
		(save-excursion
			(while (and (org-goto-sibling t)
									(save-excursion (org-up-heading-safe)))
				(when (org-entry-is-todo-p)
					(setq skip-entry t))))
		(when skip-entry
			(or (outline-next-heading)
					(goto-char (point-max))))))

(evil-define-key 'normal org-agenda-mode-map
	(kbd "RET") 'org-agenda-switch-to
	(kbd "q") 'org-agenda-quit)

;;;;; Org-latex
;; A latex-class that won't include the default packages in the generated LaTeX file. #+LATEX_CLASS: org-plain-latex.
(with-eval-after-load 'ox-latex
	(add-to-list 'org-latex-classes
							 '("org-plain-latex"
								 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
								 ("\\section{%s}" . "\\section*{%s}")
								 ("\\subsection{%s}" . "\\subsection*{%s}")
								 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
								 ("\\paragraph{%s}" . "\\paragraph*{%s}")
								 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;;; Undo-tree
(eval-and-compile(straight-use-package 'undo-tree))

(global-undo-tree-mode 1)
(setq evil-undo-system 'undo-tree)

;;;; Evil-surround
(straight-use-package 'evil-surround)
(global-evil-surround-mode 1)

;;;; Colorschemes

;;;;; Spacemacs-theme
(straight-use-package 'spacemacs-theme)

;; Set theme
(load-theme 'spacemacs-dark t)

;;;; Face
(set-face-attribute 'default nil
										:family "deja vu sans mono"
										:height my/regular-face-height)

(set-face-attribute 'mode-line nil
										:family "deja vu sans mono"
										:height my/modeline-face-height)

;; Mode specific font
(defun my/buffer-face-mode-variable ()
	"Set font to a variable width (proportional) fonts in current buffer"
	(setq buffer-face-mode-face '(:family "deja vu serif" :width semicondensed :weight regular))
	(set-face-attribute 'org-table nil :family "deja vu sans mono")
	(set-face-attribute 'org-block nil :family "deja vu sans mono")
	(buffer-face-mode))

(defun my/buffer-face-mode-fixed ()
	"Sets a fixed width (monospace) font in current buffer"
	(setq buffer-face-mode-face '(:family "deja vu sans mono"))
	(buffer-face-mode))

;; Mode specific fonts
(add-hook 'org-mode-hook 'my/buffer-face-mode-variable)

;; Modeline face
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

;; Org-mode faces
(set-face-attribute 'org-table nil :inherit 'default)
(set-face-attribute 'org-block nil :inherit 'default)
(set-face-attribute 'org-code nil :foreground "#696969")
(set-face-attribute 'org-verbatim nil :inherit 'default)

;; Ivy-highligh-face
(with-eval-after-load 'ivy
	(set-face-attribute 'ivy-highlight-face nil :inherit 'default))

;;;; Ivy
(straight-use-package 'ivy)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(ivy-mode 1)

;;;;; Counsel
(straight-use-package 'counsel)
(counsel-mode)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
;;(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x d") 'counsel-find-file)

;;;;; Swiper
(straight-use-package 'swiper)
(global-set-key (kbd "C-s") 'swiper)

;;;;; Ivy-rich
(straight-use-package 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)
(ivy-rich-project-root-cache-mode 1)

;;;; Hydra
;; https://github.com/abo-abo/hydra

(eval-and-compile (straight-use-package 'hydra))

;; | red      |                            |
;; | blue     | :exit t                    |
;; | amaranth | :foreign-keys warn         |
;; | teal     | :foreign-keys warn :exit t |
;; | pink     | :foreign-keys run          |

;;;;; Hydra-menu
(defhydra hydra-menu ()
	"Hydra Menu"
	("b" my/set-brightness "set-brightness" :exit t)
	("e" hydra-pulseaudio/body "pulseaudio-menu" :exit t)
	("s" screenshot "screenshot" :exit t)
	("s-SPC" nil "quit" :exit t))

(global-set-key (kbd "s-SPC") 'hydra-menu/body)

;;;;; Hydra-window
(defhydra hydra-window (:hint nil)
	"
Navigation                       ^^Edit             ^^^^^Resize
_C-w_: toggle    _n_: bottom-right   _c_: delete         _H_: move-left     _-_: hor-    _0_: balance
_h_:   left      _y_: top-left       _x_: xor						 _J_: move-down     _=_: hor+    _)_: area
_j_:   down                        _s_: split-below    _K_: move-up       ___: ver-
_k_:   up                          _v_: split-right    _L_: move-right    _+_: ver+
_l_:   right                       _r_: rotate
"
	("C-w" other-window :exit t)
	("c" delete-window :exit t)
	("x" delete-other-windows :exit t)
	;;("b" previous-buffer)
	;;("n" next-buffer)
	("r" evil-window-rotate-downwards)
	("s" split-window-below :exit t)
	("v" split-window-right :exit t)
	("0" balance-windows :exit t)
	(")" balance-windows-area :exit t)
	("=" (enlarge-window-horizontally 5))
	("-" (shrink-window-horizontally 5))
	("+" (enlarge-window 5))
	("_" (shrink-window 5))
	("h" evil-window-left :exit t)
	("j" evil-window-down :exit t)
	("k" evil-window-up :exit t)
	("l" evil-window-right :exit t)
	("n" evil-window-bottom-right :exit t)
	("y" evil-window-top-left :exit t)
	("H" evil-window-move-far-left :exit t)
	("J" evil-window-move-very-bottom :exit t)
	("K" evil-window-move-very-top :exit t)
	("L" evil-window-move-far-right :exit t)
	("q" nil  :exit t))

(evil-define-key 'normal 'evil-normal-state-map (kbd "C-w") 'hydra-window/body)

;;;;; Hydra-pulseaudio
(defhydra hydra-pulseaudio ()
	"pulseaudio-menu"
	("-" pulseaudio-control-decrease-volume "dec -10")
	("=" pulseaudio-control-increase-volume "inc +10")
	("_" pulseaudio-control-select-sink-by-name "select sink")
	("+" pulseaudio-control-toggle-current-sink-mute "mute sink")
	("b" hydra-menu/body "back" :exit t)
	("s-SPC" nil "quit" :exit t))

;;;;; Hydra-counsel
(defhydra hydra-counsel ()
	"counsel-menu"
	("f" counsel-describe-function "describe-function" :exit t)
	("v" counsel-describe-variable "describe-variable" :exit t)
	("l" counsel-find-library "find-library" :exit t)
	("i" counsel-info-lookup-symbol "info-lookup" :exit t)
	("u" counsel-unicode-char "unicode-char" :exit t)
	("b" hydra-menu/body "back" :exit t)
	("s-SPC" nil "quit" :exit t))

(global-set-key (kbd "C-h h") 'hydra-counsel/body)

;;;; Su
;;When `su-mode' is enabled, you will be able to edit files which you
;;lack permissions to write. `su-mode' will automatically switch the
;;visited path to a TRAMP path encoding the correct privelege
;;escalation just before you save the file.

(straight-use-package 'su)
(su-mode +1)

;;;; Smartparens
;;(straight-use-package 'smartparens)
;;(require 'smartparens-config)
;;(add-hook 'org-mode-hook #'smartparens-mode)
;;(add-hook 'prog-mode-hook #'smartparens-mode)
;;(sp-local-pair 'c-mode "'" nil :actions :rem)
;;(sp-local-pair 'c-mode "'" "'")
;;(sp-local-pair 'emacs-lisp-mode "`" "'")

;;(sp-local-pair 'org-mode "=" nil :actions :rem)
;;(setq-default sp-escape-quotes-after-insert nil)
;;Symbol's function definition is void: sp-local-pair

;;;; Ido-mode
(ido-mode -1)
;;(defun ido-mode (&optional rest)
;;	())

;;;; Envrc
(eval-and-compile
  (straight-use-package 'envrc)
  (require 'envrc))

;; Needs to be enabled as late as possible
(add-hook 'after-init-hook 'envrc-global-mode)

;;;; Exwm
(eval-and-compile (straight-use-package 'exwm))
(server-start)
(require 'exwm)

(setq exwm-workspace-number 1)

(add-hook 'exwm-update-class-hook
					(lambda ()
						(unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
												(string= "gimp" exwm-instance-name))
							(exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
					(lambda ()
						(when (or (not exwm-instance-name)
											(string-prefix-p "sun-awt-X11-" exwm-instance-name)
											(string= "gimp" exwm-instance-name))
							(exwm-workspace-rename-buffer exwm-title))))

(setq exwm-input-global-keys
			`(
				;; [s-r] Exit char-mode and fullscreen mode
				([?\s-r] . exwm-reset)
				;; [s-w] Switch workspace interactively
				([?\s-w] . exwm-workspace-switch)
				;; [s-%d] Switch to a workspace by its index
				,@(mapcar (lambda (i)
										`(,(kbd (format "s-%d" i)) .
											(lambda ()
												(interactive)
												(exwm-workspace-switch-create ,i))))
									(number-sequence 0 9))
				;; [s-&][M-&] Launch applications
				([?\s-&] . (lambda (command)
										 (interactive (list (read-shell-command "$ ")))
										 (start-process-shell-command command nil command)))
				;; Bind "s-<f2>" to start mupdf on this pdf file
				([s-f2] . (lambda ()
										(interactive)
										(start-process "" nil "/usr/bin/mupdf" (f-this-file))))))

(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
(define-key exwm-mode-map [?\s-q] #'exwm-input-release-keyboard)
(define-key exwm-mode-map [?\s-f] #'exwm-layout-toggle-fullscreen)

(define-key exwm-mode-map [?\C-w] #'hydra-window/body)
(define-key exwm-mode-map [?\s-\ ] #'hydra-menu/body)
(define-key exwm-mode-map [?\s-a] #'org-agenda)
(define-key exwm-mode-map [?\s-c] #'org-capture)

(define-key exwm-mode-map [?\s-h] #'previous-buffer)
(define-key exwm-mode-map [?\s-l] #'next-buffer)
(define-key exwm-mode-map (kbd "C-<tab>") #'my/toggle-buffer)

(evil-set-initial-state 'exwm-mode 'emacs)
(setq exwm-input-simulation-keys
			'(
				;; movement
				([?\C-u] . [prior])
				([?\C-d] . [next])))

(defun exwm-passthrough (orig-fun keymap on-exit &optional foreign-keys)
	(setq exwm-input-line-mode-passthrough t)
	(let ((on-exit (let ((on-exit on-exit))
									 (lambda ()
										 (setq exwm-input-line-mode-passthrough nil)
										 (when on-exit (funcall on-exit))))))
		(apply orig-fun keymap on-exit (list foreign-keys))))

(advice-add 'hydra-set-transient-map :around #'exwm-passthrough)

(require 'exwm-randr)
(exwm-randr-enable)
(exwm-enable)

;;; Packages

;;;; Magit
(straight-use-package 'magit)

;;;;; Keybinds
(evil-define-key 'normal 'evil-normal-state-map
	(kbd "C-x g") 'magit-status)
(evil-define-key 'normal magit-mode-map
	(kbd "RET") 'magit-diff-dwim
	(kbd "j") 'magit-section-forward
	(kbd "k") 'magit-section-backward
	(kbd "F") 'magit-pull
	(kbd "s") 'magit-stage-file
	(kbd "u") 'magit-unstage-file
	(kbd "c") 'magit-commit
	(kbd "m") 'magit-merge
	(kbd "P") 'magit-push
	(kbd "f") 'magit-fetch
	(kbd "l") 'magit-log
	(kbd "i") 'magit-gitignore
	(kbd "r") 'magit-refresh
	(kbd "g") 'beginning-of-buffer
	(kbd "G") 'end-of-buffer
	(kbd "M") 'magit-remote
	(kbd "d") 'magit-diff
	(kbd "b") 'magit-branch
	(kbd "R") 'magit-reset
	(kbd "Q") 'magit-mode-bury-buffer)

;;;; Flyspell
(add-hook 'org-mode-hook 'flyspell-mode)

;;;; Olivetti
(straight-use-package 'olivetti)
(add-hook 'text-mode-hook (lambda () (setq olivetti-body-width 100)))
(add-hook 'text-mode-hook 'olivetti-mode)
(add-hook 'prog-mode-hook (lambda () (setq olivetti-body-width 0.8)))
(add-hook 'prog-mode-hook 'olivetti-mode)

;;;; Dired
(add-hook 'dired-mode-hook
					(lambda ()
						(dired-hide-details-mode)))
;; Human readable memory listing
(setq dired-listing-switches "-alh")

;;;; Helm
(straight-use-package 'helm)

;;;; Avy
(straight-use-package 'avy)
(evil-define-key 'normal 'evil-normal-state-map (kbd "C-a") 'evil-avy-goto-char-timer)

;;;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

;;;; Which Key
(straight-use-package 'which-key)
(which-key-mode)
(setq which-key-show-prefix 'left)

;;;; Outshine
(straight-use-package 'outshine)
(add-hook 'prog-mode 'outshine-mode)
(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(setq outshine-fontify-whole-heading-line t)

;;;; Hide-mode-line
(straight-use-package 'hide-mode-line)

;;;; Image-mode
(setq image-auto-resize 'fit-height)
(evil-set-initial-state 'image-mode 'normal)
(evil-define-key 'normal image-mode-map
	(kbd "W") 'image-transform-fit-to-width
	(kbd "H") 'image-transform-fit-to-height
	(kbd "j") 'image-scroll-up
	(kbd "k") 'image-scroll-down
	(kbd "l") 'image-next-file
	(kbd "h") 'image-previous-file)

;;;; Completion Engines

;;;;; Yasnippet
(straight-use-package 'yasnippet)
(add-to-list 'load-path
						 "~/.emacs.d/plugins/yasnippet")
(yas-global-mode 1)

;;;;; Company
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-other-buffers t
			company-dabbrev-code-other-buffers t)

;;;; Org-bullets
(straight-use-package 'org-bullets)
(defun org-bullet-mode()
	(org-bullets-mode 1))
(add-hook 'org-mode-hook 'org-bullet-mode)

;;;; Org-bars
(eval-and-compile	(straight-use-package
									 '(org-bars :type git :host github :repo "tonyaldon/org-bars")))
(require 'org-bars)
(add-hook 'org-mode-hook #'org-bars-mode)
(setq org-bars-stars '(:empty "◉"
															:invisible "→"
															:visible "↘"))
;;;; Aggressive Indent
(straight-use-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;;;; Pdf-tools
(load-library "pdf-tools-autoloads")
(pdf-tools-install)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;;;; Keybinds
(evil-define-key 'normal pdf-view-mode-map
	(kbd "j") 'pdf-view-scroll-up-or-next-page
	(kbd "k") 'pdf-view-scroll-down-or-previous-page
	(kbd "C-j") 'pdf-view-next-line-or-next-page
	(kbd "C-k") 'pdf-view-previous-line-or-previous-page
	(kbd "J") 'pdf-view-next-page-command
	(kbd "K") 'pdf-view-previous-page-command
	(kbd "h") 'image-backward-hscroll
	(kbd "l") 'image-forward-hscroll
	(kbd "f") 'pdf-view-goto-page
	(kbd "r") 'pdf-view-revert-buffer
	(kbd "=") 'pdf-view-enlarge
	(kbd "+") 'pdf-view-enlarge
	(kbd "-") 'pdf-view-shrink
	(kbd "0") 'pdf-view-scale-reset
	(kbd "H") 'pdf-view-fit-height-to-window
	(kbd "W") 'pdf-view-fit-width-to-window
	(kbd "P") 'pdf-view-fit-page-to-window
	(kbd "/") 'isearch-forward-word
	(kbd "n") 'isearch-repeat-forward
	(kbd "N") 'isearch-repeat-backward
	(kbd "G") 'pdf-view-first-page
	(kbd "o") 'pdf-outline)

;;;; Rainbow Delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;; Gnuplot
(straight-use-package 'gnuplot)
;;;; Ox-twbs
(straight-use-package 'ox-twbs)

;;;; Flycheck
(straight-use-package 'flycheck)
(global-flycheck-mode)
(with-eval-after-load 'flycheck
	(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;; Elpy
(straight-use-package 'elpy)
(elpy-enable)

;;;; Org-babel
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)
														 (C . t)))
;;;; My/toggle-buffer
(defun my/toggle-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-<tab>") 'my/toggle-buffer)

;;;; My/set-brightness
(defun my/set-brightness()
	(interactive)
	(setq my/max-brightness-file "/sys/class/backlight/intel_backlight/max_brightness")
	(setq my/brightness-file "/sys/class/backlight/intel_backlight/brightness")

	(let* ((my/max-brightness
					(string-to-number(f-read-text my/max-brightness-file)))
				 (my/brightness-ratio
					(/ (string-to-number(read-from-minibuffer "Brightness 0-100: ")) 100.0))
				 (my/brightness
					(floor(* my/max-brightness my/brightness-ratio))))
		(shell-command
		 (concat "echo " (number-to-string my/brightness) " > " my/brightness-file))))

;;;; Pulseaudio-control
(straight-use-package 'pulseaudio-control)

;;;; ScreenShot
(straight-use-package 'screenshot)
(global-set-key (kbd "s-s") 'screenshot)
(my/directory-p-nil-create "~/Media/Screenshots")
(my/directory-p-nil-create "~/Org/references")
(setq screenshot-schemes
			'(
				("Media-dir"
				 :dir "~/Media/Screenshots")
				("Roam-dir"
				 :dir "~/Org/references")
				("current-dir"
				 :dir default-directory)))

;;;; Randr-config
(defun generate-randr-config (primary secondary)
	(-flatten `(,(-map (lambda (n) (list n primary)) (number-sequence 1 7))
							(0 secondary)
							,(-map (lambda (n) (list n secondary)) (number-sequence 8 9)))))

(defun randr-layout-dp1-extend ()
	"Extend the screen to Display Port"

	(interactive)
	(setq exwm-randr-workspace-monitor-plist (generate-randr-config "DP-1" "eDP-1"))
	(exwm-randr-refresh)
	(randr-layout-single)
	(shell-command "xrandr --output DP-1 --left-of eDP-1 --auto --primary"))

(defun randr-layout-hdmi1-extend ()
	"Extend the screen to HDMI"

	(interactive)
	(setq exwm-randr-workspace-monitor-plist (generate-randr-config "HDMI-1" "eDP-1"))
	(exwm-randr-refresh)
	(randr-layout-single)
	(shell-command "xrandr --output HDMI-1 --auto --left-of eDP-1 --auto --primary"))

(defun randr-layout-hdmi1-only ()
	"Only HDMI is shown"

	(interactive)
	(exwm-randr-refresh)
	(shell-command "xrandr --output HDMI-1 --auto --primary")
	(shell-command "xrandr --output eDP-1 --off"))

(defun randr-layout-dp1-only ()
	"Only HDMI is shown"

	(interactive)
	(exwm-randr-refresh)
	(shell-command "xrandr --output DP-1 --auto --primary")
	(shell-command "xrandr --output eDP-1 --off"))

(defun randr-layout-single ()
	"Laptop screen only!"

	(interactive)
	(exwm-randr-refresh)
	(shell-command "xrandr --output eDP-1 --auto --primary")
	(shell-command "xrandr --output HDMI-1 --off")
	(shell-command "xrandr --output DP-1 --off"))

;; xrandr multiple monitor
;;(setq exwm-randr-workspace-output-plist '(1 "DP-1" 2 "DP-2"))
;;(add-hook 'exwm-randr-screen-change-hook
;;(lambda ()
;;(start-process-shell-command
;;"xrandr" nil "xrandr --output DP-1 --right-of LVDS1 --auto")))

;;;; Libvterm
(load-library "vterm-autoloads")

;;;; Projectile
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)

;;;; Org-roam
(straight-use-package 'org-roam)

(setq org-roam-v2-ack t)

(with-eval-after-load 'org
	(require 'org-roam))

(my/directory-p-nil-create "~/Org/roam-repo/")

(setq org-roam-directory "~/Org/roam-repo/")

(defvar org-roam-capture-templates)
(setq org-roam-capture-templates
			'(("d" "default" plain
				 "%?"
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
														"#+TITLE: ${title}\n#+CREATED: %u\n#+Time-stamp: \" \"")
				 :unnarrowed t)
				("b" "book-note" plain
				 "\n\n* Bibliography\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n %?"
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
														"#+title: ${title}\n#+CREATED: %u\n#+Time-stamp: \" \"")
				 :unnarrowed t)))

(defun org-roam-node-insert-immediate (arg &rest args)
	(interactive "P")
	(let ((args (cons arg args))
				(org-roam-capture-templates (list (append (car org-roam-capture-templates)
																									'(:immediate-finish t)))))
		(apply #'org-roam-node-insert args)))

;;(global-set-key (kbd "C-c n i") 'org-roam-node-insert-immediate)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n t") 'org-roam-buffer-toggle)

(org-roam-db-autosync-mode)

;;;; Bibliography
(my/directory-p-nil-create "~/Org/bibliography/")

;;;; Ebib
;; Creating / Editing bib files
(eval-and-compile (straight-use-package 'ebib))
(require 'ebib)
(setq ebib-preload-bib-files '("~/Org/bibliography/bibliography.bib"))

(setq ebib-layout 'window
			ebib-truncate-file-names nil)

;;;; Org-roam-bibtex (ORB)
;; Integration of org-roam + ivy-bibtex + org-ref
(eval-and-compile (straight-use-package 'org-roam-bibtex))
(require 'org-ref)
(org-roam-bibtex-mode 1)

;;;;; Bibtex
;; Generation of entry key for a bibtex entry, See bibtex-generate-autokey
(setq bibtex-autokey-year-title-separator ":")

;;;;; Org-ref
(setq reftex-default-bibliography '("~/Org/bibliography/bibliography.bib")
			org-ref-default-bibliography '("~/Org/bibliography/bibliography.bib")
			org-ref-pdf-directory "~/Org/bibliography/files/")

;;;;; Helm-bibtex
;; List of bibliography files
(straight-use-package 'helm-bibtex)
(setq bibtex-completion-bibliography
			'("~/Org/bibliography/bibliography.bib"))
(setq bibtex-completion-pdf-field "file")

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
				 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
				(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

;;;; Org-noter
(straight-use-package 'org-noter)

;;;; Org-roam-ui
(straight-use-package
 '(org-roam-ui :type git :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out")))

(global-set-key (kbd "C-c n g") org-roam-ui-mode)

(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)

;;;; Deft
(straight-use-package 'deft)
(global-set-key (kbd "C-c n d") 'deft)
(setq deft-recursive t
			deft-use-filter-string-for-filename t
			deft-default-extension "org"
			deft-directory org-roam-directory)

(defun cm/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
		(if begin
				(string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
			(deft-base-filename file))))

(advice-add 'deft-parse-title :override #'cm/deft-parse-title)

(setq deft-strip-summary-regexp
			(concat "\\("
							"[\n\t]" ;; blank
							"\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
							"\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
							"\\)"))

;;;; Anki
(straight-use-package 'anki-editor)
(setq anki-editor-create-decks t)

;;;; Message-mode
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; kill message buffer after message is sent
(setq message-kill-buffer-on-exit t)

;;;; My/insert-current-date-time
(defvar my/current-date-format "%F"
  "Format of date to insert with `insert-current-date' func
See help of `format-time-string' for possible replacements")

(defvar my/current-time-format "%a %H:%M:%S %Z"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun my/insert-current-date ()
  "insert the current date and time into current buffer.
Uses `current-date-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string my/current-date-format (current-time)))
  (insert "\n"))

(defun my/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string my/current-time-format (current-time)))
  (insert "\n"))

(defun my/insert-current-date-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string my/current-time-format (current-time)))
	(insert " ")
  (insert (format-time-string my/current-date-format (current-time)))
	(insert "\n"))

;;; Mode Line
(setq display-time-load-average nil)
(setq display-time-format "%a %d/%b %R ")
(setq battery-mode-line-format "%L %p%% %t")

;; Count buffer-local number of lines function
(defun my/modeline-line-number-max ()
	(setq count-number-of-lines
				(format "%d" (line-number-at-pos (point-max))))
	(make-local-variable 'count-number-of-lines)
	(force-mode-line-update))
(add-hook 'window-configuration-change-hook 'my/modeline-line-number-max)
(add-hook 'after-save-hook 'my/modeline-line-number-max)

;;;; Mode-line alignment
;; Set modeline width
(defun my/calc-modeline-width()
	(setq my/modeline-total-width (floor(* (window-total-width nil 'floor) my/modeline-face-factor)))
	(make-local-variable 'my/modeline-total-width))

(add-hook 'exwm-workspace-switch-hook 'my/calc-modeline-width)
(add-hook 'window-configuration-change-hook 'my/calc-modeline-width)
(add-hook 'window-state-change-hook 'my/calc-modeline-width)

;; Write a function to do the spacing
(defun simple-mode-line-render (left right)
	"Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
	(let ((available-width
				 (max 0 (- my/modeline-total-width
									 (+ (length (format-mode-line left))
											(length (format-mode-line right)))))))
		(append left
						(list (format (format "%%%ds" available-width) ""))
						right)))

(setq-default mode-line-format
							'((:eval
								 (simple-mode-line-render
									;; Left.
									(quote (" "
													mode-line-mule-info
													mode-line-modified
													" "
													mode-name
													" %b "
													vc-mode
													))
									;; Right.
									(quote ("  L%l:"
													count-number-of-lines
													" | "
													battery-mode-line-string
													" | "
													display-time-string
													""))))))
