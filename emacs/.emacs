;;;; Package Configurantion

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Install and Configure Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;;; Essentials

(use-package ivy
  :config
  (use-package flx)
  (use-package smex)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("C-c j" . counsel-git-grep)
   :map	minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package counsel
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

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :defer t
  :bind
  (("C-x g" . magit-status)))

(use-package pdf-tools
  :defer t
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-resize-factor 1.1)
  :init
  (pdf-loader-install))


;;;; Org Mode

(use-package org
  :pin org
  :ensure org-plus-contrib
  :defer t
  :config
  (setq org-startup-indented t)
  (setq org-default-notes-file "~/core/org/gtd/inbox.org")
  (setq org-archive-location "~/core/org/gtd/inbox.org::")
  (setq org-refile-targets (quote ((nil :maxlevel . 6)
                                 (org-agenda-files :maxlevel . 6))))
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/org/gtd/inbox.org")
	   "* TODO %?\n %i\n")))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-preview-latex-image-directory "~/.emacs.d/ltximg")
  (add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))

  ;; Org agenda
  (setq org-agenda-files '("~/core/org/gtd/"))
  (setq org-agenda-custom-commands
	'(("d" "Show scheduled study drills."agenda ""
	   ((org-agenda-files
	     (directory-files-recursively "~/core/learning/" org-agenda-file-regexp))
	    (org-agenda-entry-types '(:scheduled))
	    (org-agenda-span 'week)))))

  ;; Extra Org modules
  (add-to-list 'org-modules  'org-habit t)
  (use-package org-drill)
  (use-package org-bullets
    :hook
     (org-mode . (lambda () (org-bullets-mode 1))))
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda))


;;;; Programming

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))


;;; Racket
(use-package racket-mode
  :defer t)


;;;; Miscellaneous Config Options

(set-face-attribute 'default nil :height 160)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

(setq delete-by-moving-to-trash t)

(setq require-final-newline t)

(setq vc-follow-symlinks t)

(put 'dired-find-alternate-file 'disabled nil)

(setq doc-view-resolution 400)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
