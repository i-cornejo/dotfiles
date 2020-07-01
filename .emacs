;; Package Configurantion
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package helm
  :init
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  :config
  (require 'helm-config))

;; Org Mode
(require 'org-drill)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/core/org/inbox.org")

(setq org-agenda-files
      '("~/core/org/gtd"))

(setq org-agenda-custom-commands
  '(("d" "Show scheduled study drills." agenda ""
     ((org-agenda-files '("~/archivos_pink/autodidact/courses/notes/"))
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

;; Set docview resolution to a higher dpi (default 100)
(setq doc-view-resolution 300)
;; Set custom variables directory and load it
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Enable a for opening directories replacing current dired buffer
(put 'dired-find-alternate-file 'disabled nil)

