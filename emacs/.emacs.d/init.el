;;;; Package Configuration

(require 'package)
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

(use-package no-littering)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file t)

;;;; Gotta go fast - Taken from Doom Emacs

(setq gc-cons-threshold most-positive-fixnum
      idle-update-delay 1.0
      load-prefer-newer nil
      auto-mode-case-fold nil
      fast-but-imprecise-scrolling t
      initial-major-mode 'fundamental-mode) ; do not trigger prog-mode-hook

(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)

(setq my/file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist my/file-name-handler-alist)))

(use-package gcmh
  :defer 3
  :config
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024)))


;;;; Essentials

;;; Help

(use-package which-key
  :defer 3
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
  :defer t
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
	  (counsel-rg . ivy--regex-ignore-order)
	  (t . ivy--regex-fuzzy))))

(use-package counsel
  :defer 3
  :bind
  ("C-x C-f" . counsel-find-file)
  ("C-x f" . counsel-recentf)
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
(use-package amx
  :defer t)

(use-package recentf
  :defer t
  :ensure f
  :config
  (setq recentf-max-menu-items 100
	recentf-max-saved-items 100)
  (add-to-list 'recentf-exclude no-littering-var-directory
	       'recentf-exclude no-littering-etc-directory))

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

(add-hook 'emacs-startup-hook #'winner-mode)

;;; Utilities

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
	([f11] . nil)
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
	org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  ;; Org agenda
  (setq org-agenda-files '("~/org/gtd/")
	org-archive-location "~/org/gtd/inbox.org::"
	org-log-into-drawer t
	org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 3)
				   ("~/org/roam/notes/UNAM" :tag . "UNAM"))))

  ;; Org Capture Templates
  (setq org-default-notes-file "~/org/gtd/inbox.org"
	org-capture-templates
	'(("i" "Inbox" entry (file "")
	   "* TODO %?\n")
	  ("h" "Homework" entry (file+headline "~/org/gtd/gtd.org" "Homework")
	   "* TODO %? :hw:\n")
	  ("t" "Tasks" entry (file+headline "~/org/gtd/gtd.org" "Tasks")
	   "* TODO %?\n")))

  ;; Extra Org modules
  (add-to-list 'org-modules  'org-habit t))

(use-package org-plus-contrib
  :defer t)

(use-package org-pomodoro
  :defer t)

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

;;; Org Roam

(use-package org-roam
  :bind
  ("C-c n n" . org-roam-jump-to-index)
  ("C-c n f" . org-roam-find-file)
  :config
  (setq org-roam-buffer-window-parameters '((no-delete-other-windows . t))
	org-roam-index-file "~/org/roam/index.org"
	org-roam-db-location (no-littering-expand-var-file-name "org-roam.db")
	org-roam-directory "~/org/roam")

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
	      (("C-c n i" . org-roam-insert-immediate))))

(use-package deft
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam/"))

;;;; Programming

;;; General

(setq dabbrev-check-all-buffers nil
      show-paren-delay  0
      show-paren-style 'mixed)

(add-hook 'emacs-startup-hook #'show-paren-mode)

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
  (setq company-selection-wrap-around t))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically
	'(save mode-enabled)))

(use-package yasnippet
  :defer t
  :config
  (yas-minor-mode))

(use-package ivy-xref
  :defer t
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs
	xref-show-xrefs-function #'ivy-xref-show-xrefs)
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
  (python-mode . lsp-deferred)
  :commands
  (lsp lsp-deferred)
  :bind
  (:map lsp-mode-map
	("M-?" . lsp-find-references))
  :config
  (setq read-process-output-max (* 1024 1024)
	lsp-completion-enable-additional-text-edit nil
	lsp-enable-on-type-formatting nil
	lsp-keep-workspace-alive nil))

(use-package dap-mode
  :defer t
  :config
  (setq dap-auto-configure-features
	'(sessions locals controls tooltip))
  (dap-auto-configure-mode))

;;; C/C++

(use-package ccls
  :config
  (setq-default flycheck-disabled-checkers
		'(c/c++-clang c/c++-cppcheck c/c++-gcc))
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

(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))

(setq python-indent-guess-indent-offset-verbose nil)

;;; Racket

(use-package racket-mode
  :defer t)

;;; R

(use-package ess
  :defer t
  :config
  (setq ess-use-flymake nil))


;;;; Visual Niceties

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-fontset-font
 t 'symbol "Noto Color Emoji" nil 'append)

(setq display-time-default-load-average nil
      display-time-format " | %I:%M %p")
(add-hook 'after-init-hook #'display-time-mode)

(setq battery-mode-line-format "%b%p%% "
      battery-load-critical 30)
(add-hook 'after-init-hook #'display-battery-mode)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package minions
  :hook
  (emacs-startup . minions-mode)
  :config
  (setq minions-mode-line-lighter "-"
	minions-mode-line-delimiters nil
	minions-direct '(flycheck-mode)))

(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq x-underline-at-descent-line t))


;;;; Security for the intrepid

(setq ffap-machine-p-known 'reject)

;;;; Miscellaneous

(setq ring-bell-function 'ignore
      require-final-newline t
      vc-follow-symlinks t
      doc-view-resolution 400
      initial-scratch-message nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Backups

(setq backup-directory-alist
      `((".*" . ,(no-littering-expand-var-file-name "backup/")))
      vc-make-backup-files t
      backup-by-copying-when-linked t
      delete-old-versions t
      auto-save-list-file-prefix nil
      auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Enable functions

(put 'dired-find-alternate-file 'disabled nil)

;; Custom functions

(defun my/days-count ()
  "Show remaining days and weeks until event"
  (interactive)
  (let ((days-remaining (org-time-stamp-to-now "<2021-02-20 Thu>")))
    (message (format "Days: %d - Weeks: %d" days-remaining (ceiling (/ days-remaining 7.0))))))
