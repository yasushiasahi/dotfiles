;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; leafの初期設定 https://github.com/conao3/leaf.el#install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf リーフの準備をするぞ
    :init
    (leaf leaf-keywords :ensure t
      :init
      (leaf hydra :ensure t)
      ;; (leaf el-get :ensure t)
      ;; (leaf blackout :ensure t)
      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init))
    (leaf leaf-tree :ensure t)
    (leaf leaf-convert :ensure t)
    (leaf macrostep :ensure t
      :doc "対話形式でマクロを展開する"
      :url "https://github.com/joddie/macrostep")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ここから設定を書いていく
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf 美しい見た目にするぞ
  :init
  ;; titlebarに入れる文字列
  (setq frame-title-format "%b")

  (leaf solarized-theme :ensure t
    :doc "Solarizedテーマを適用"
    :url "http://github.com/bbatsov/solarized-emacs"
    :custom ((solarized-emphasize-indicators . nil)
             (solarized-use-less-bold . t))
    :init
    (load-theme 'solarized-dark t))

  (leaf mini-modeline :ensure t
    :doc "モードラインをミニバッファに表示する"
    :url "https://github.com/kiennq/emacs-mini-modeline"
    :global-minor-mode t)

  (leaf fira-code-mode :ensure t
    :doc "FiraCodeのリガチャをいい感じに設定してくれる"
    :url "https://github.com/jming422/fira-code-mode"
    :custom ((fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")))
    :hook prog-mode-hook))

(leaf 細々した内部的な設定
  :init
  (leaf no-littering :ensure t
    :doc "自動生成される設定ファイルの保存先をverとetcに限定する"
    :url "https://github.com/emacscollective/no-littering"
    :config
    (leaf recentf
      :doc "最近開いたファイルの履歴を保存する"
      :global-minor-mode t
      :custom ((recentf-max-saved-items . 2000))
      :defvar (recentf-exclude)
      :config
      (setq no-littering-etc-directory
	    (expand-file-name "etc/" user-emacs-directory))
      (setq no-littering-var-directory
	    (expand-file-name "var/" user-emacs-directory))
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory))

    (leaf cus-start
      :doc "編集中のファイルのバックアップを作成する"
      :custom `((auto-save-file-name-transforms . '((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

    (leaf cus-edit
      :doc "init.el内にcustom-set-variablesのダンプを吐かせないようにする"
      :custom `((custom-file . ,(no-littering-expand-var-file-name "custom.el"))))))

(leaf 使いやすいキーバインドにするぞ
  :custom ((mac-option-modifier . 'meta)
	   (mac-command-modifier . 'super))
  :bind (;; コマンドキーでコピペ操作
	 ("s-v" . yank)
	 ("s-c" . kill-ring-save)
	 ("s-x" . kill-region)
	 ;; カーソル移動
	 ("C-a" . my-beginning-of-line-text-or-line))
  :init
  ;; C-hはバックスペースにする https://www.emacswiki.org/emacs/BackspaceKey
  (define-key key-translation-map [?\C-?] [?\C-h])
  (define-key key-translation-map [?\M-\d] [?\M-h])
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\M-h] [?\M-\d])

  (defun my-beginning-of-line-text-or-line ()
    "行の最初の文字の位置に移動。すでに最初の文字だったら行頭に移動。"
    (interactive)
    (let ((curr-point (point))		  ; コマンド実行前のカーソル位置
	  (curr-column (current-column))) ; コマンド実行前の行番号
      (beginning-of-line-text)		  ; 一旦行の最初の文字の位置に移動
      (when (and (/= curr-column 0)	  ; 元々行頭にいなかった
		 (<= curr-point (point))) ; 最初の文字の位置よりも前にいた
	(beginning-of-line)))))		  ; その場合は行頭に移動

(leaf 一般的な便利機能をいい感じに設定するぞ
  :init
  (leaf delete_trailing_whitespace_before_save
    :doc "保存する前に行末スペースを消す"
    :hook ((before-save-hook . delete-trailing-whitespace)))

  (leaf delsel
    :doc "リージョン選択状態でyankした時にリージョンを上書きする"
    :global-minor-mode delete-selection-mode)

  (leaf smartparens :ensure t
    :doc "カッコの対応関係をいい感じにしてくれる"
    :url "https://github.com/Fuco1/smartparens"
    :require smartparens-config
    :hook prog-mode-hook)

  (leaf expand-region :ensure t
    :doc "選択範囲をいい感じに拡大していくことができる"
    :bind (("C-=" . er/expand-region)))

  (leaf rainbow-delimiters :ensure t
    :doc "カッコの対応関係をわかりやすく色つけする"
    :url "https://github.com/Fanael/rainbow-delimiters"
    :hook prog-mode-hook)

  (leaf vundo :ensure t
    :doc "変更履歴を木構造で表示して行き来できる。undo-treeの横表示バージョン"
    :url "https://github.com/casouri/vundo"
    :bind (("C-x u" . vundo)))

  (leaf which-key :ensure t
    :doc "Display available keybindings in popup"
    :url "https://github.com/justbur/emacs-which-key"
    :global-minor-mode t)

  (leaf window_customize
    :doc "window操作を直感的に行う"
    :bind (("C-t" . hydra-window/body))
    :init
    (defhydra hydra-window nil
      ("b" windmove-left)
      ("n" windmove-down)
      ("p" windmove-up)
      ("f" windmove-right)
      ("<left>" windmove-left)
      ("<down>" windmove-down)
      ("<up>" windmove-up)
      ("<right>" windmove-right)
      ("C-<left>" shrink-window-horizontally)
      ("C-<right>" enlarge-window-horizontally)
      ("C-<up>" shrink-window)
      ("C-<down>" enlarge-window)
      ("s" window-swap-states)
      ("-" split-window-below)
      ("\\" split-window-right)
      ("e" balance-windows)
      ("d" delete-window)
      ("1" delete-other-windows))))

(leaf 最強の補完インターフェイスを作り上げるぞ
  :init
  (leaf vertico :ensure t
    :doc "ミニバッファに垂直に候補を表示して、インタラクティブに選択出来るUIを提供する"
    :url "https://github.com/minad/vertico"
    :global-minor-mode t
    :config
    ;; mini-modeline-modeの背景と色がかぶるのでカレントの見た目を調整
    (set-face-attribute 'vertico-current nil :underline t :bold t)

    (leaf savehist
      :doc "ミニバッファの表示履歴を保存する"
      :global-minor-mode t))

  (leaf consult :ensure t
    :doc "補完コマンドを提供する"
    :url "https://github.com/minad/consult"
    :bind (;; C-x bindings (ctl-x-map)
	   ("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
	   ;; Other custom bindings
           ("M-y" . consult-yank-pop)
	   ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flycheck)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-g o" . consult-outline)
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
	   ;; M-s bindings (search-map)
	   ("M-s l" . consult-line)
           ("M-s r" . consult-ripgrep)
	   ("M-s f" . my-consult-fd)
	   ("M-s g r" . my-consult-ghq-ripgrep)
	   ("M-s g f" . my-consult-ghq-fd))
    :config
    (leaf orderless :ensure t
      :doc "補完候補絞込のいい感じのスタイルを提供する"
      :url "https://github.com/oantolin/orderless"
      :custom ((completion-styles . '(orderless basic))))

    (leaf marginalia :ensure t
      :doc "補完候補の詳細を表示する"
      :url "https://github.com/minad/marginalia"
      :global-minor-mode t)

    (leaf consult-flycheck :ensure t
      :doc "Provides the command `consult-flycheck'"
      :url "https://github.com/minad/consult")

    (leaf consult-lsp :ensure t
      :doc "LSP-mode Consult integration"
      :url "https://github.com/gagbo/consult-lsp")

    (leaf my-consult-fd
      :doc "consult-findのfd版 https://github.com/minad/consult/wiki#find-files-using-fd"
      :defun (consult--fd-builder consult--find consult--directory-prompt consult--command-split consult--join-regexps my-consult-fd--builder)
      :defvar (consult--regexp-compiler)
      :init
      (defun my-consult-fd--builder (input)
	"my-consult-fdの設定みたいなもの"
	(pcase-let* ((`(,arg . ,opts) (consult--command-split input))
		     (`(,re . ,hl) (funcall consult--regexp-compiler
					    arg 'extended t)))
	  (when re
	    (list :command (append
			    (list "fd" "--color=never" "--full-path" "--hidden"
				  (consult--join-regexps re 'extended))
			    opts)
		  :highlight hl))))

      (defun my-consult-fd (&optional dir initial)
	"my-consult-fdの本体"
	(interactive "P")
	(let* ((prompt-dir (consult--directory-prompt "Fd" dir))
               (default-directory (cdr prompt-dir)))
	  (find-file (consult--find (car prompt-dir) #'my-consult-fd--builder initial)))))

    (leaf my-consult-ghq
      "ghqの結果をconsultに渡す"
      :defun (my-consult-ghq--read my-consult-ghq--list-candidates consult--read)
      :init
      (defun my-consult-ghq--list-candidates ()
	"ghq listの結果をリストで返す"
	(with-temp-buffer
	  (unless (zerop (apply #'call-process "ghq" nil t nil '("list" "--full-path")))
	    (error "Failed: Cannot get ghq list candidates"))
	  (let ((paths))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (push
	       (buffer-substring-no-properties
		(line-beginning-position) (line-end-position))
	       paths)
	      (forward-line 1))
	    (nreverse paths))))

      (defun my-consult-ghq--read ()
	"ghq管理のリポジトリ一覧から選ぶ"
	(consult--read
	 (my-consult-ghq--list-candidates)
	 :prompt "ghq: "
	 :category 'file))

      (defun my-consult-ghq-fd ()
	"ghq管理のリポジトリ一覧から選び、プロジェクト内ファイル検索"
	(interactive)
	(my-consult-fd (my-consult-ghq--read)))

      (defun my-consult-ghq-ripgrep ()
	"ghq管理のリポジトリ一覧から選び、プロジェクト内でripgrep"
	(interactive)
	(consult-ripgrep (my-consult-ghq--read)))))

  (leaf corfu :ensure t
    :doc "オートコンプリートインターフェイス"
    :url "https://github.com/minad/corfu"
    :custom ((corfu-count . 30)
	     (corfu-auto . t)
	     (corfu-cycle . t)
	     (corfu-quit-no-match . t)
	     (corfu-auto-prefix . 2))
    :bind (:corfu-map
	   ("C-f" . corfu-insert))
    :init
    (global-corfu-mode)
    :config
    (leaf corfu-doc :ensure t
      :doc "Corfuの選択対象のドキュメントを表示"
      :url "https://github.com/galeo/corfu-doc"
      :bind (:corfu-map
	     ("M-p" . corfu-doc-scroll-down)
	     ("M-n" . corfu-doc-scroll-up))
      :hook corfu-mode-hook)))

(leaf プログラミングを快適にするユーティリティだぞ
  :init
  ;; https://zenn.dev/hyakt/articles/6ff892c2edbabb#%E8%A8%AD%E5%AE%9A
  (leaf tree-sitter :ensure (t tree-sitter-langs)
    :doc "各言語に上質なシンタックスハイライトを適用する"
    :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
    :defvar (tree-sitter-major-mode-language-alist)
    :require tree-sitter-langs
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
    ;; TSXの対応
    (tree-sitter-require 'tsx)
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
    ;; ハイライトの追加
    (tree-sitter-hl-add-patterns 'tsx
      [
       ;; styled.div``
       (call_expression
        function: (member_expression
                   object: (identifier) @function.call
                   (.eq? @function.call "styled"))
        arguments: ((template_string) @property.definition
                    (.offset! @property.definition 0 1 0 -1)))
       ;; styled(Component)``
       (call_expression
        function: (call_expression
                   function: (identifier) @function.call
                   (.eq? @function.call "styled"))
        arguments: ((template_string) @property.definition
                    (.offset! @property.definition 0 1 0 -1)))
       ]))

  (leaf add-node-modules-path :ensure t
    :doc "Add node_modules to your exec-path"
    :url "https://github.com/codesuki/add-node-modules-path"
    :hook ((typescript-mode-hook typescript-tsx-mode-hook js-mode-hook) . add-node-modules-path))

  (leaf prettier-js :ensure t
    :doc "Minor mode to format JS code on file save"
    :url "https://github.com/prettier/prettier-emacs"
    :hook typescript-tsx-mode-hook typescript-mode-hook js-mode-hook)

  (leaf flycheck :ensure t
    :doc "On-the-fly syntax checking"
    :url "http://www.flycheck.org"
    :init
    (global-flycheck-mode))

  (leaf lsp-mode :ensure t
    :doc "LSP mode"
    :url "https://github.com/emacs-lsp/lsp-mode"
    :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
           ((typescript-mode-hook typescript-tsx-mode-hook) . lsp-deferred))
    :custom `(;; (gc-cons-threshold . 100000000)
              ;; (read-process-output-max . 1048576)
	      ;; (lsp-idle-delay . 0.5)
              ;; (lsp-keymap-prefix . "C-c l")
              ;; (lsp-enable-indentation . nil)
              ;; (lsp-headerline-breadcrumb-enable . nil)
              ;; rust
              ;; (lsp-rust-analyzer-cargo-watch-command . "clippy")
              ;; (lsp-rust-analyzer-proc-macro-enable . t)
              ;; (lsp-rust-analyzer-experimental-proc-attr-macros . t)
              ;; (lsp-rust-analyzer-server-display-inlay-hints . t)
	      ;; eslint
	      (lsp-eslint-server-command . '("node"
					     ,(concat (getenv "HOME") "/ghq/github.com/microsoft/vscode-eslint/server/out/eslintServer.js")
					     "--stdio"))
	      )
    :defvar lsp-file-watch-ignored-directories
    :config
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.next\\'")

    (leaf lsp-ui :ensure t
      :doc "UI modules for lsp-mode"
      :url "https://github.com/emacs-lsp/lsp-ui")

    ;; (leaf lsp-tailwindcss :ensure t
    ;;   :doc "A lsp-mode client for tailwindcss"
    ;;   :url "https://github.com/merrickluo/lsp-tailwindcss")
    )




  )






(leaf 快適にプログラミングできるようにするぞ
  :init
  (leaf typescript-mode :ensure t
    :doc "Major mode for editing typescript"
    :url "http://github.com/ananthakumaran/typescript.el"
    :preface
    (define-derived-mode typescript-tsx-mode typescript-mode "React")
    :mode ("\\.ts\\'"
           ("\\.tsx\\'" . typescript-tsx-mode))
    :custom ((typescript-indent-level . 2)))

  (leaf fish-mode :ensure t
    :doc "Fishモード"
    :hook `((fish-mode-hook . ,(lambda ()
				 (add-hook 'before-save-hook 'fish_indent-before-save)))))

  (leaf yaml-mode :ensure t
    :doc "Major mode for editing YAML files"
    :url "https://github.com/yoshiki/yaml-mode"
    :bind (:yaml-mode-map
	   ("C-m" . newline-and-indent)))





  )


;;; init.el ends here
