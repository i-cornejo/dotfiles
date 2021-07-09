;; early-init.el --- Early Init File

;;; Frame Configuration
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;;; No splash screens
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
