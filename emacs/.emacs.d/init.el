;;; init.el --- icmor Emacs configuration file

;;; Commentary

;; Got inspired to start a new Emacs journey based on the
;; philosophy of Protesilaos Stavrou. Basically, put in
;; the effort to discover Emacs the "Emacs Way". Sacrificing
;; temporary convenience in exchange of really learning
;; the tools I use and achieving autonomy through power,
;; knowledge and humility.

;;; Package Configuration
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;;; No littering
(setq user-var-dir	 (concat user-emacs-directory "var/"))
(setq user-etc-dir	 (concat user-emacs-directory "etc/"))

(setq bookmark-default-file		 (concat user-var-dir "bookmark-default.el"))
(setq speed-type-gb-dir			 (concat user-var-dir "speed-type/"))
(setq image-dired-dir			 (concat user-var-dir "image-dired/"))
(setq tramp-persistency-file-name	 (concat user-var-dir "tramp/persistency.el"))
(setq custom-file			 (concat user-etc-dir "custom.el"))
(load custom-file)

;;;; Backups and Auto-save files
(setq auto-save-list-file-prefix (concat user-var-dir "auto-save-list/.saves-"))
(setq backup-directory-alist `((".*" . ,(concat user-var-dir "backup"))))
(setq tramp-backup-directory-alist backup-directory-alist)

;;; Functions
(defun my/vterm-toggle ()
  "Toggle a vterm window"
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (delete-window)
    (vterm-other-window)))

(defun my/toggle-habit-length ()
  "Toggle wheter to display 21 or 90 days for habits"
  (interactive)
  (if (eq org-habit-preceding-days 21)
      (setq org-habit-preceding-days 90)
    (setq org-habit-preceding-days 21))
  (org-agenda-redo))

;;; General Bindings
(global-set-key (kbd "C-z") #'my/vterm-toggle)
(global-set-key (kbd "C-x f") #'find-file)

;;; vterm
(add-hook 'vterm-mode-hook
	  (lambda () (local-unset-key (kbd "C-z"))))
(eval-after-load 'vterm
  '(progn
     (define-key vterm-mode-map (kbd "C-u") #'vterm--self-insert)
     (define-key vterm-mode-map (kbd "C-SPC") #'vterm-copy-mode)))

;;; Comint
(setq shell-command-prompt-show-cwd t)
(setq comint-prompt-read-only t)

;;; Org-mode
(setq org-agenda-files '("~/org/gtd/"))
(setq org-log-into-drawer t)
(add-hook 'org-mode-hook 'visual-line-mode)
(eval-after-load 'org '(add-to-list 'org-modules 'org-habit t))
(global-set-key (kbd "C-c a") #'org-agenda-list)
(eval-after-load 'org-agenda
  '(define-key org-agenda-mode-map (kbd "h") #'my/toggle-habit-length))

;;; Visual
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-14"))
(load-theme 'monokai t)

;;; Miscellaneous
(winner-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq find-file-suppress-same-file-warnings t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq sentence-end-double-space nil)
(setq vc-follow-symlinks nil)
(setq delete-by-moving-to-trash t)
(setq ring-bell-function 'ignore)
(setq disabled-command-function nil)
