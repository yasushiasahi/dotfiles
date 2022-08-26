;;; early-init.el --- Loaded before the package system and GUI is initialized
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Code:

;; Solarizedのテーマが読み込まれるまでの一瞬白背景がちらつくのを抑制
(set-face-attribute 'default nil :background "#002b36")


;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)                 ; only for mouse events
(setq use-file-dialog nil)
(setq initial-scratch-message nil)
(setq x-underline-at-descent-line t)

(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

(set-face-attribute 'default nil
                    :family "Fira Code")

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation

;;; early-init.el ends here
