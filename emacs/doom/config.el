;;-*- lexical-binding: t -*-

(setq user-full-name "Iñaki Cornejo"
      user-mail-address "cornejodlm@gmail.com")

(setq doom-font (font-spec :family "monospace" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 18)
      doom-unicode-font (font-spec :font  "Noto Color Emoji"))

(setq doom-theme 'doom-molokai)

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(setq +ivy-buffer-preview t)

(toggle-frame-fullscreen)

(setq delete-by-moving-to-trash t)

(setq ivy-read-action-function #'ivy-hydra-read-action)

(use-package! org
  :config
  (add-to-list 'org-modules 'org-habit t)
  (setq
   org-agenda-files '("~/org/gtd")
  org-archive-location "~/org/gtd/inbox.org::"
  org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
  org-agenda-custom-commands
	'(("d" "Show scheduled study drills."agenda ""
	   ((org-agenda-files
	     (directory-files-recursively "~/learn/" org-agenda-file-regexp))
	    (org-agenda-entry-types '(:scheduled))
	    (org-agenda-span 'week))))
  org-capture-templates
	'(("i" "Inbox" entry (file "~/org/gtd/inbox.org")
	   "* TODO %?\n %i\n")
	  ("h" "Homework" entry (file+headline "~/org/gtd/gtd.org" "Homework")
	   "* TODO %? :hw:\n %i\n")
	  ("t" "Tasks" entry (file+headline "~/org/gtd/gtd.org" "Tasks")
	   "* TODO %?\n %i\n")
	  ("v" "Vocabulary" entry (file "~/org/zettel/notes/vocabulary")))))

(after! org-roam
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
	     :unnarrowed t))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
