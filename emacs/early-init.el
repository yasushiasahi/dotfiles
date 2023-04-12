;;; early-init.el --- Loaded before the package system and GUI is initialized
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:




;; GUIを消す
(menu-bar-mode -1)			; メニューバーを表示しない
(tool-bar-mode -1)			; ツールバーを表示しない
(scroll-bar-mode -1)			; スクロールバーを表示しない

(custom-set-variables
 '(inhibit-startup-echo-area-message t)	  ; 起動時にエコーエリアに挨拶文を表示しない
 '(inhibit-startup-screen t)		  ; 起動時のデフォルト画面を表示しない
 '(initial-scratch-message nil)		  ; *scratch*バッファのデフォルト文章を表示しない
 '(scroll-preserve-screen-position t)	  ; 画面がスクロールする時にカーソルを画面上の位置で固定する
 '(scroll-conservatively 1)		  ; 1行ずつスクロールする
 '(create-lockfiles nil)		  ; 編集中のファイルのロックファイル(.#~~)を作らない
 '(delete-old-versions t)		  ; 古いバックアップファイルを確認なしで消す
 '(truncate-lines t)			  ; 行を折り返さない
 '(x-underline-at-descent-line t)	  ; アンダーラインの位置をいい感じにする。solarized-emacsで推奨されている https://github.com/bbatsov/solarized-emacs#underline-position-setting-for-x
 '(native-comp-async-report-warnings-errors 'silent) ; ネイティブコンパイルのwarningsをbufferに記録するがポップアップはさせない。
 '(gc-cons-threshold 100000000)			     ; ガベージコレクションが発火するメモリの閾値 https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
 '(read-process-output-max (* 1024 1024))	     ; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
 '(frame-title-format "")                      ; titlebarを""にする（何も表示しない）
 '(ring-bell-function 'ignore)                 ; 警告音（ピープ音）をならさい
 )

(set-face-attribute 'default nil :family "FiraCode Nerd Font")

;; Mac portでは必要ない https://github.com/railwaycat/homebrew-emacsmacport/wiki/Natural-Title-Bar
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; emacs 29から設定しておくとよいらしい https://github.com/emacscollective/no-littering#native-compilation-cache
;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;; 	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))


;;; early-init.el ends here
