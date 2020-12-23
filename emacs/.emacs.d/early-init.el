;;;; Frame Configuration

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq initial-frame-alist '((fullscreen . fullboth)))
(set-face-attribute 'default nil :height 180)
