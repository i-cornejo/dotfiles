;;;; Frame Configuration

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(fullscreen . fullboth) default-frame-alist)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 165)
