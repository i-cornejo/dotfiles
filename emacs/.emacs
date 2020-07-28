;; Package Configurantion
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'use-package-ensure)

;; Ensure all packages are installed
(setq use-package-always-ensure t)

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
  (("\C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("C-c j" . counsel-git-grep)
   :map	minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package which-key
  :config
  (which-key-mode))

(use-package magit
  :defer t
  :bind
  (("C-x g" . magit-status)))

(use-package pdf-tools
  :defer t
  :init
  (pdf-loader-install))

(use-package racket-mode
  :defer t)

;; Org Mode
(setq org-startup-indented t)
(add-hook 'org-mode-hook #'visual-line-mode)

(use-package org
  :pin org
  :defer t
  :config
  (add-to-list 'org-modules  'org-habit t))

(use-package org-drill)

(use-package org-bullets
  :defer t)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/core/org/gtd/inbox.org")
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files
      '("~/core/org/gtd/"))

(setq org-agenda-custom-commands
      '(("d" "Show scheduled study drills."agenda ""
	 ((org-agenda-files '("~/core/org/notes/"))
	  (org-agenda-entry-types '(:scheduled))
	  (org-agenda-start-day "nil")
	  (org-agenda-span 'week)
	  (org-agenda-include-diary nil)
	  (org-agenda-show-all-dates t)))))

(setq org-refile-targets (quote ((nil :maxlevel . 6)
                                 (org-agenda-files :maxlevel . 6))))

(setq org-archive-location "~/core/org/logs/log.org::")

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/gtd/inbox.org")
         "* TODO %?\n %i\n")))

;; Follow symlinks to their files without asking
(setq vc-follow-symlinks t)

;; Set font size
(set-face-attribute 'default nil :height 160)

;; Set color scheme
(use-package monokai-theme)
(load-theme 'monokai t)

;; Show trailing whitespace for programming and Org
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Disable tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable beeping
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Change backup directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;; Delete files by moving them to the trash
(setq delete-by-moving-to-trash t)

;; Set custom variables directory and load it
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Enable 'a' for opening file/dir in current buffer
(put 'dired-find-alternate-file 'disabled nil)
