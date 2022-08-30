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

(setq inhibit-startup-echo-area-message nil) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; mac portでは必要ない
;; https://github.com/railwaycat/homebrew-emacsmacport/wiki/Natural-Title-Bar
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

(set-face-attribute 'default nil :family "Cica" :height 140)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation

;; emacs 29 から設定しておくとよい
;; https://github.com/emacscollective/no-littering#native-compilation-cache
;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;; 	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
