;; Package Configurantion
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  :bind
  (("\C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   :map	minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org-bullets
  :ensure t
  :defer t)

(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (pdf-loader-install))

(use-package racket-mode
			 :ensure t
			 :defer t)

;; Org Mode
(setq org-startup-indented t)
(add-hook 'org-mode-hook #'visual-line-mode)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :config
  (add-to-list 'org-modules 'org-habit t))

(use-package org-drill
  :commands (org-drill))

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/core/org/inbox.org")
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files
      '("~/core/org/"))

(setq org-agenda-custom-commands
  '(("d" "Show scheduled study drills." agenda ""
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
      '(("t" "Todo" entry (file "~/org/inbox.org")
         "* TODO %?\n %i\n")))

;; Set font size
(set-face-attribute 'default nil :height 160)

;; Set color scheme
(use-package monokai-theme :ensure t)
(load-theme 'monokai t)

;; Show trailing whitespace
(setq show-trailing-whitespace t)

;; Enable a for opening directories replacing current dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Disable tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

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

