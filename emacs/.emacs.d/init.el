;;;; Gotta go fast

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)


;;;; Package Configuration

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq byte-compile-warnings '(cl-functions)) ; package cl is deprecated warning

;;;; Custom Settings

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;;; Essentials

;;; Help

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  :hook
  (emacs-lisp-mode . (lambda ()
		       (local-set-key (kbd "C-c C-d") #'helpful-at-point)))
  :config
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable))

(use-package elisp-demos
  :hook
  (helpful-mode . (lambda ()
		    (advice-add 'helpful-update
				:after #'elisp-demos-advice-helpful-update))))

;;; General Completion

(use-package ivy
  :defer t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-read-action-function #'ivy-hydra-read-action
	ivy-initial-inputs-alist nil
	ivy-re-builders-alist
	'((swiper-isearch . ivy--regex-ignore-order)
	  (t . ivy--regex-fuzzy))))

(use-package counsel
  :bind
  ("C-x C-f" . counsel-find-file)
  ("C-x r b" . counsel-bookmark)
  ("M-x" . counsel-M-x)
  ("C-c j j" . counsel-rg)
  ("C-c j i" . counsel-imenu)
  ("C-c j l" . counsel-find-library)
  :config
  (counsel-mode)
  (ivy-configure 'counsel-imenu
    :update-fn 'auto))

(use-package swiper
  :bind
  ("C-s" . swiper-isearch))

(use-package ivy-hydra :defer t)
(use-package flx :defer t)
(use-package amx :defer t)

;;; Window Management

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-background nil
	aw-scope 'frame
	aw-dispatch-always t
	aw-reverse-frame-list t
	ace-window-display-mode t))

(winner-mode)

;;; Utilities

(use-package gcmh
  :hook
  (after-init . gcmh-mode))

(use-package pdf-tools
  :defer t
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-continuous nil)
  :init
  (pdf-loader-install))

(use-package vterm
  :bind
  (:map vterm-mode-map
	("C-u" . vterm--self-insert)))
  
(use-package vterm-toggle
  :bind*
  ([f2] . vterm-toggle)
  :config
  (setq vterm-toggle-hide-method 'reset-window-configration)
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
	(let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
		     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action))

;;;; Org Mode

(use-package org
  :pin org
  :hook
  (org-mode . visual-line-mode)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  :config
  (setq org-startup-indented t
	org-return-follows-link t
	org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
	org-preview-latex-image-directory "~/.emacs.d/ltximg/")

  ;; Org agenda
  (setq org-agenda-files '("~/org/gtd/")
	org-archive-location "~/org/gtd/inbox.org::"
	org-log-into-drawer t
	org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 3)
				   ("~/org/roam/notes/UNAM" :tag . "UNAM")))
	org-agenda-custom-commands
	'(("d" "Show scheduled study drills."agenda ""
	   ((org-agenda-files
	     (directory-files-recursively "~/org/roam/notes/" org-agenda-file-regexp))
	    (org-agenda-entry-types '(:scheduled))
	    (org-agenda-span 'week)))))

  ;; Org Capture Templates
  (setq org-default-notes-file "~/org/gtd/inbox.org"
	org-capture-templates
	'(("i" "Inbox" entry (file "")
	   "* TODO %?\n")
	  ("h" "Homework" entry (file+headline "~/org/gtd/gtd.org" "Homework")
	   "* TODO %? :hw:\n")
	  ("t" "Tasks" entry (file+headline "~/org/gtd/gtd.org" "Tasks")
	   "* TODO %?\n")
	  ("m" "Matemáticas" entry (file+headline "~/org/roam/notes/unam.org" "Matemáticas")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("e" "Español" entry (file+headline "~/org/roam/notes/unam.org" "Español")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("f" "Física" entry (file+headline "~/org/roam/notes/unam.org" "Física")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("q" "Química" entry (file+headline "~/org/roam/notes/unam.org" "Química")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("b" "Biología" entry (file+headline "~/org/roam/notes/unam.org" "Biología")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("h" "Historia" entry (file+headline "~/org/roam/notes/unam.org" "Historia Universal")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("m" "México" entry (file+headline "~/org/roam/notes/unam.org" "Historia de México")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("x" "Literatura" entry (file+headline "~/org/roam/notes/unam.org" "Literatura")
	   "* TODO %^{Todo} :drill:\n%?\n")
	  ("g" "Geografía" entry (file+headline "~/org/roam/notes/unam.org" "Geografía")
	   "* TODO %^{Todo} :drill:\n%?\n")))

  ;; Extra Org modules
  (add-to-list 'org-modules  'org-habit t))

(use-package org-plus-contrib
  :defer t)

(use-package org-drill
  :commands
  (org-drill))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

;;; Org Roam

(use-package org-roam
  :bind
  ("C-c n n" . org-roam-jump-to-index)
  ("C-c n f" . org-roam-find-file)
  :config
  (setq org-roam-directory "~/org/roam")
  (org-roam-mode)
  (setq org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (setq org-roam-index-file "~/org/roam/index.org")

  ;; Templates
  (setq org-roam-capture-templates
	'(("p" "permanent" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)
	  ("n" "note" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "notes/${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)))

  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))


;;;; Programming

;;; General

(defvar my/programming-modes '(emacs-lisp-mode
			       c-mode
			       c++-mode
			       python-mode
			       web-mode
			       racket-mode
			       java-mode))

(setq dabbrev-check-all-buffers nil
      show-paren-delay  0
      show-paren-style 'mixed)
(show-paren-mode)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (markdown-mode . ws-butler-mode)
  (org-mode . ws-butler-mode))

(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-selection-wrap-around t
	company-global-modes my/programming-modes))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
	flycheck-global-modes my/programming-modes))

(use-package ivy-xref
  :defer t
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :config
  (ivy-configure 'ivy-xref-show-defs
    :update-fn 'auto)
  (ivy-configure 'ivy-xref-show-xrefs
    :update-fn 'auto))

;;; Git

(use-package magit
  :bind
  (("C-x g" . magit-status)))

;;; LSP

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  (lsp lsp-deferred)
  :config
  (setq read-process-output-max (* 1024 1024)
	lsp-completion-enable-additional-text-edit nil
	lsp-enable-on-type-formatting nil))

(use-package dap-mode
  :defer t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-auto-configure-mode))

;;; C/C++

(use-package ccls
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook
  ((c-mode c++-mode) . lsp-deferred))

;;; Java

(use-package lsp-java
  :hook
  (java-mode . lsp-deferred))

;;; Web

(use-package web-mode
  :hook
  (sgml-mode . web-mode))

(use-package emmet-mode
  :hook
  (sgml-mode . emmet-mode))

;;; Python

(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
	pythonic-interpreter "python3"))

(use-package company-anaconda
  :defer t)

(add-hook 'python-mode-hook
	  (lambda () (add-to-list 'company-backends
				  '(company-anaconda :with company-capf))))

;;; Racket

(use-package racket-mode
  :defer t)

;;; R

(use-package ess
  :defer t
  :config
  (setq ess-use-flymake nil))

(setq initial-major-mode 'fundamental-mode) ; do not trigger prog-mode-hook

;;;; Visual Niceties

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(setq display-time-default-load-average nil
      display-time-format " | %I:%M %p")
(display-time-mode)
(setq battery-mode-line-format "%b%p%% ")
(setq battery-load-critical 30)
(display-battery-mode)

(use-package minions
    :config
    (setq minions-mode-line-lighter "-"
	minions-mode-line-delimiters nil
	minions-direct '(flycheck-mode))
    (minions-mode))

(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq x-underline-at-descent-line t))


;;;; Miscellaneous

(setq ring-bell-function 'ignore
      backup-directory-alist
      '((".*" . "~/.emacs.d/backup"))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      delete-old-versions t
      load-prefer-newer t
      require-final-newline t
      vc-follow-symlinks t
       doc-view-resolution 400)

(put 'dired-find-alternate-file 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)
