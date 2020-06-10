;; Package Configurantion
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  :config
  (require 'helm-config)
  (helm-mode 1))

;; Set font size
(set-face-attribute 'default nil :height 160)

;; Set color scheme
(load-theme 'monokai t)

;; Show trailing whitespace
(setq show-trailing-whitespace t)

;; Disable tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Disable beeping
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Change backup directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;; Set custom variables directory and load it
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Enable a for opening directories replacing current dired buffer
(put 'dired-find-alternate-file 'disabled nil)

