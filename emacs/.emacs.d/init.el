;;;; Package Configurantion

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

;; Custom Settings

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Install and Configure Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package diminish)


;;;; Essentials

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (use-package flx)
  (use-package smex)
    (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
    :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("C-c j" . counsel-git-grep)
   :map	minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :diminish
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-reverse-frame-list t)
  (ace-window-display-mode t))

(winner-mode 1)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package pdf-tools
  :defer t
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-continuous nil)
  :init
  (pdf-loader-install))

(use-package vterm
  :config
  (use-package vterm-toggle
    :bind*
    ([f2] . vterm-toggle))
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
  :ensure org-plus-contrib
  :hook
  (org-mode . visual-line-mode)
  :config
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (eval-after-load 'org-indent '(diminish 'org-indent-mode))
  (setq org-default-notes-file "~/core/org/gtd/inbox.org")
  (setq org-archive-location "~/core/org/gtd/inbox.org::")
  (setq org-refile-targets (quote ((nil :maxlevel . 6)
                                 (org-agenda-files :maxlevel . 6))))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-preview-latex-image-directory "~/.emacs.d/ltximg/")

  ;; Org agenda
  (setq org-agenda-files '("~/core/org/gtd/"))
  (setq org-agenda-custom-commands
	'(("d" "Show scheduled study drills."agenda ""
	   ((org-agenda-files
	     (directory-files-recursively "~/core/learning/" org-agenda-file-regexp))
	    (org-agenda-entry-types '(:scheduled))
	    (org-agenda-span 'week)))))

  ;; Org Capture Templates
  (setq org-capture-templates
	'(("i" "Inbox" entry (file+headline "~/core/org/gtd/inbox.org" "Inbox")
	   "* TODO %?\n %i\n")
	("h" "Homework" entry (file+headline "~/core/org/gtd/gtd.org" "Homework")
	   "* TODO %?\n %i\n")
	("t" "Tasks" entry (file+headline "~/core/org/gtd/gtd.org" "Tasks")
	   "* TODO %?\n %i\n")))


  ;; Org Roam
  (use-package org-roam
    :init
    (setq org-roam-directory "~/core/org/zettel")
    (org-roam-mode)
    :diminish
    :config
    (setq org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
    (setq org-roam-index-file "~/core/org/zettel/index.org")

    ;; Templates
    (setq org-roam-capture-templates
	 '(("p" "permanent" plain (function org-roam--capture-get-point)
	    "%?"
	    :file-name "${slug}"
	    :head "#+title: ${title}\n"
	    :unnarrowed t)
	 ("c" "fleeting" plain (function org-roam--capture-get-point)
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


  ;; Extra Org modules
  (add-to-list 'org-modules  'org-habit t)
  (use-package org-drill)
  (use-package org-bullets
    :hook
    (org-mode . (lambda () (org-bullets-mode 1))))
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c n n" . org-roam-jump-to-index)
  ("C-c n f" . org-roam-find-file))


;;;; Programming

(global-set-key (kbd "M-/") 'hippie-expand)
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(setenv "VIRTUALENVWRAPPER_PYTHON" "/usr/bin/python3")
(setenv "WORKON_HOME"
	"/home/pink/archivos_pink/programs/python/.virtualenvs")

(use-package ws-butler
  :diminish
  :hook
  (prog-mode . ws-butler-mode)
  (org-mode . ws-butler-mode))

(use-package company
  :config
  (setq company-selection-wrap-around t)
  (setq company-idle-delay nil)
  :diminish
  :bind*
  ("C-M-i" . company-manual-begin)
  :hook
  (after-init . global-company-mode))

(use-package flycheck
 :defer t
 :config
(setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package yasnippet
  :diminish
  :hook
  (java-mode . yas-minor-mode))


(use-package lsp-mode
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-prefer-flymake nil)
  (use-package lsp-ivy)
  (use-package lsp-treemacs)
  (use-package lsp-ui
    :config
    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-doc-delay 1.5))
  (use-package dap-mode
    :config
    (setq dap-auto-configure-features '(sessions locals controls)))
  :commands
  (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (java-mode . lsp-deferred))

;;; Python

(use-package elpy
  :config
  (setq python-shell-interpreter "ipython3"
	python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-rpc-python-command "python3")
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
  :hook
  (elpy-mode . (lambda () (setq company-idle-delay nil)))
  (elpy-mode . flycheck-mode)
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;;; Java

(use-package lsp-java
  :defer t
  :config
  (use-package dap-java
    :ensure nil))

;;; Racket
(use-package racket-mode
  :defer t)

;;; R

(use-package ess
  :config
  (setq ess-use-flymake nil)
  :hook
  (ess-mode . (lambda () (flycheck-mode t)))
  :init
  (require 'ess-r-mode))

;;;; Miscellaneous Config Options

(custom-set-faces '(trailing-whitespace ((t (:background "dim gray")))))
(diminish 'visual-line-mode)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(setq visible-bell t
      ring-bell-function 'ignore
      backup-directory-alist
      '((".*" . "~/.emacs.d/backup"))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      delete-old-versions t
      delete-by-moving-to-trash t
      electric-pair-mode 1
      require-final-newline t
      python-indent-guess-indent-offset-verbose nil
      vc-follow-symlinks nil
      doc-view-resolution 400)

(put 'dired-find-alternate-file 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)
