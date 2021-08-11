;;; init.el --- icmor's Emacs
;;;; Commentary
;; Got inspired to start a new Emacs journey based on the
;; philosophy of Protesilaos Stavrou. Basically, put in
;; the effort to discover Emacs the "Emacs Way". Sacrificing
;; temporary convenience in exchange of really learning
;; the tools I use and achieving autonomy through power,
;; knowledge and humility.

;;; No littering
;;;; General
(setq user-var-dir	 (concat user-emacs-directory "var/"))
(setq user-etc-dir	 (concat user-emacs-directory "etc/"))

;;;; Backups and Auto-save files
(setq auto-save-list-file-prefix (concat user-var-dir "auto-save-list/.saves-"))
(setq backup-directory-alist `((".*" . ,(concat user-var-dir "backup"))))
(setq tramp-backup-directory-alist backup-directory-alist)

;;;; Litter
(setq bookmark-default-file		(concat user-var-dir "bookmark-default.el"))
(setq custom-file			(concat user-etc-dir "custom.el"))
(setq image-dired-dir			(concat user-var-dir "image-dired/"))
(setq project-list-file			(concat user-var-dir "projects"))
(setq savehist-file			(concat user-var-dir "savehist"))
(setq speed-type-gb-dir			(concat user-var-dir "speed-type/"))
(setq tramp-persistency-file-name	(concat user-var-dir "tramp/persistency.el"))
(setq transient-history-file		(concat user-var-dir "transient/history.el"))
(setq transient-levels-file		(concat user-etc-dir "transient/levels.el"))
(setq transient-values-file		(concat user-etc-dir "transient/values.el"))
(setq url-cache-directory		(concat user-var-dir "url/cache/"))
(setq url-configuration-directory	(concat user-var-dir "url/configuration/"))

;;; Package Configuration
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(load custom-file t)
(package-install-selected-packages)

;;; Functions
(defun my/vterm-toggle ()
  "Toggle a vterm window"
  (interactive)
  (if (eq major-mode 'vterm-mode)
	  (previous-buffer)
      (call-interactively #'vterm)))

;;; Global Bindings
(global-set-key [f2] #'my/vterm-toggle)
(global-set-key (kbd "C-c @ @") #'outline-minor-mode)
(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "C-x l") #'count-words)
(if (display-graphic-p)
    (global-unset-key (kbd "C-z")))

;;; Org-mode
;;;; General
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/gtd/"))
(setq org-refile-targets '((nil . (:level . 1))
			   (org-agenda-files . (:level . 1))))
(setq org-log-into-drawer t)
(setq org-return-follows-link t)
;;;; Visual
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-adapt-indentation nil)
(add-hook 'org-mode-hook (lambda () (setq fill-column 100)))

;;;; Agenda
(eval-after-load 'org '(add-to-list 'org-modules 'org-habit t))
(global-set-key (kbd "C-c a") #'org-agenda-list)
(global-set-key (kbd "C-c c") #'org-capture)

;;;; Capture
(setq org-capture-templates
      '(("t" "Tasks" entry (file+headline "gtd/gtd.org" "Tasks")
         "* TODO %?\n")
	("h" "Homework" entry (file+headline "gtd/gtd.org" "Homework")
         "* TODO %?\n")
	("e" "Events" entry (file+headline "gtd/gtd.org" "Events")
         "* TODO %?\n")
	("i" "Inbox" entry (file+headline "gtd/inbox.org" "Inbox")
	 "* TODO %?\n")
	("j" "Journal" plain (file+olp+datetree "life/journal.org")
	 "%?")))

;;; Essentials
;;;; Dired
(setq dired-dwim-target t)
(setq dired-hide-details-hide-symlink-targets nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;;; Which-Key
(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0.05)
(which-key-mode)

;;;; Vterm
(setq vterm-max-scrollback 10000)
(eval-after-load 'vterm
  '(progn
     (define-key vterm-mode-map (kbd "C-u") #'vterm--self-insert)
     (define-key vterm-mode-map (kbd "C-SPC") #'vterm-copy-mode)
     (define-key vterm-mode-map (kbd "C-M-v") nil)
     (define-key vterm-mode-map (kbd "C-S-M-v") nil)
     (define-key vterm-mode-map [f2] nil)))

;;;; Magit
(global-set-key (kbd "C-x g") 'magit)

;;;; Marginalia
(marginalia-mode)

;;;; Pdf-Tools
(pdf-loader-install)

;;;; Pomodoro
(eval-after-load 'pomodoro
  '(pomodoro-add-to-mode-line))
(setq pomodoro-sound-player "paplay")
(setq pomodoro-work-start-sound
      "/usr/share/sounds/freedesktop/stereo/phone-outgoing-busy.oga")
(setq pomodoro-break-start-sound
      "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga")

;;;; IRC
(setq rcirc-default-nick "icmor")
(setq rcirc-default-user-name "icmor")
(setq rcirc-server-alist
      '(("irc.libera.chat"
	 :port 6697
	 :encryption tls)))

;;; Programming
;;;; General
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(setq outline-minor-mode-cycle t)
(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;;;; Comint
(setq shell-command-prompt-show-cwd t)
(setq comint-prompt-read-only t)

;;;; C
(add-hook 'c-mode-hook
	  (lambda () (c-set-style "k&r")))

;;; Miscellaneous
;;;; Visual
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-14"))
(load-theme 'modus-vivendi t)
(setq bookmark-fontify nil)
(blink-cursor-mode -1)
(tooltip-mode -1)

;;;; Completion
(setq read-buffer-completion-ignore-case t)
(setq completions-format 'one-column)

;;;; Window Management
(winner-mode)

;;;; History
(savehist-mode 1)
(setq history-length 1000)
(setq history-delete-duplicates t)

;;;; Documents
(setq doc-view-resolution 400)

;;;; Text
(prefer-coding-system 'utf-8)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq save-interprogram-paste-before-kill t)
(setq-default fill-column 80)

;;;; Files
(setq auto-save-default nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq vc-follow-symlinks nil)
(setq delete-by-moving-to-trash t)

;;;; Etc
(setq use-short-answers t)
(setq initial-scratch-message nil)
(setq find-file-suppress-same-file-warnings t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq ring-bell-function 'ignore)
(setq disabled-command-function nil)
(setq auth-source-save-behavior 'never)
