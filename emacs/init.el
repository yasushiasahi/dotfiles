;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el と leafの初期設定 https://github.com/conao3/leaf.el#install
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

  (leaf leaf
    :doc "ご存じleaf"
    :url "https://github.com/conao3/leaf.el"
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
;;; 見た目
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :custom-face (mini-modeline-mode-line . '((t :background "#008b8b" :height 0.1 :box nil)))
  :custom ((mini-modeline-face-attr . nil))
  :global-minor-mode t)

(leaf fira-code-mode :ensure t
  :doc "FiraCodeのリガチャをいい感じに設定してくれる"
  :url "https://github.com/jming422/fira-code-mode"
  :custom ((fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")))
  :hook prog-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 自動生成ファイルの出力場所
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf no-littering :ensure t
  :doc "自動生成される設定ファイルの保存先をverとetcに限定する"
  :url "https://github.com/emacscollective/no-littering"
  :require t
  :config
  (leaf recentf
    :doc "最近開いたファイルの履歴を保存する"
    :global-minor-mode t
    :custom ((recentf-max-saved-items . 100))
    :defvar (recentf-exclude)
    :config
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))

  (leaf savehist
    :doc "ミニバッファの表示履歴を保存する"
    ;; :custom `((savehist-file . ,(no-littering-expand-var-file-name "history")))
    :global-minor-mode t)

  (leaf cus-start
    :doc "編集中のファイルのバックアップを作成する"
    :custom `((auto-save-file-name-transforms . '((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

  ;; (leaf files
  ;;   :doc "オープン時(編集前)のファイルをバックアップを作成する"
  ;;   :custom `((backup-directory-alist . '((".*" . ,(no-littering-expand-var-file-name "backup/"))))))

  (leaf cus-edit
    :doc "init.el内にcustom-set-variablesのダンプを吐かせないようにする"
    :custom `((custom-file . ,(no-littering-expand-var-file-name "custom.el")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド、カーソル移動
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf mac
  :doc "Mac Portの設定"
  :custom ((mac-option-modifier . 'meta)     ; commandキーをsuperとして扱う
           (mac-command-modifier . 'super))) ; optionキーをmetaとして扱う

(leaf simple
  :doc "カーソル"
  :bind (;; コマンドキーでコピペ操作
	       ("s-v" . yank)
	       ("s-c" . kill-ring-save)
	       ("s-x" . kill-region)
	       ;; カーソル移動
         ("C-o" . my-insertline-with-indent)
	       ("C-a" . my-beginning-of-line-text-or-line))
  :hook ((before-save-hook . delete-trailing-whitespace)) ; 保存する前に行末スペースを消す
  :init
  ;; C-hはバックスペースにする https://www.emacswiki.org/emacs/BackspaceKey
  (define-key key-translation-map [?\C-?] [?\C-h])
  (define-key key-translation-map [?\M-\d] [?\M-h])
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\M-h] [?\M-\d])

  (defun my-insertline-with-indent (&optional arg)
    "ポイントよりも右側を下の行に送りその前にインデントを入れる"
    (interactive "P")
    (let ((num (or arg 1)))        ; 引数がなければnumは1
      (newline-and-indent num)          ; num行分インデントしながら改行する
      (forward-line (* num -1))         ; num行分戻る
      (move-end-of-line nil)))          ; 行の一番後ろへ移動

  (defun my-beginning-of-line-text-or-line ()
    "行の最初の文字の位置に移動。すでに最初の文字だったら行頭に移動。"
    (interactive)
    (let ((curr-point (point))		        ; コマンド実行前のカーソル位置
	        (curr-column (current-column))) ; コマンド実行前の行番号
      (back-to-indentation)		            ; 一旦行の最初の文字の位置に移動
      (when (and (/= curr-column 0)	      ; 元々行頭にいなかった
		             (<= curr-point (point))) ; 最初の文字の位置よりも前にいた
	      (beginning-of-line))))            ; その場合は行頭に移動
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 一般的な便利機能
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)       ; yesではなくyと打てる
(setq use-dialog-box nil)               ; ダイアログmacosのダイアログではなく、yes-or-noを使う

(leaf autorevert
  :doc "開いているfileがEmacs外で更新された時、EmacsのBufferも更新する"
  :custom ((global-auto-revert-non-file-buffers . t)) ; diradとかfileバッファでないものも更新する
  :config
  (global-auto-revert-mode 1))

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

(leaf rainbow-mode :ensure t
  :doc "Colorize color names in buffers"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :global-minor-mode t)

(leaf vundo :ensure t
  :doc "変更履歴を木構造で表示して行き来できる。undo-treeの横表示バージョン"
  :url "https://github.com/casouri/vundo"
  :bind (("C-x u" . vundo)))

(leaf wgrep :ensure t
  :doc "Writable grep buffer and apply the changes to files"
  :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el")

(leaf which-key :ensure t
  :doc "Display available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :global-minor-mode t)

(leaf window-management
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
    ("0" delete-window)
    ("1" delete-other-windows)))

;; shut up, emacs!
(leaf multiple-cursors :ensure t
  :doc "Multiple cursors for Emacs."
  :url "https://github.com/magnars/multiple-cursors.el"
  :bind (("C-q" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("|" mc/vertical-align)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; フィジーファインダー、オートコンプリート
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf orderless :ensure t
  :doc "補完候補絞込のいい感じのスタイルを提供する"
  :url "https://github.com/oantolin/orderless"
  :custom ((completion-styles . '(orderless basic))
           (completion-category-defaults . nil)
           (completion-category-overrides . nil)))

(leaf marginalia :ensure t
  :doc "補完候補の詳細を表示する"
  :url "https://github.com/minad/marginalia"
  :bind ((:minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  :global-minor-mode t)

(leaf vertico :ensure t
  :doc "ミニバッファに垂直に候補を表示して、インタラクティブに選択出来るUIを提供する"
  :url "https://github.com/minad/vertico"
  :custom ((vertico-count . 30)
	         (vertico-resize . 't)
	         (vertico-cycle . t))
  :global-minor-mode t)

(leaf consult :ensure t
  :doc "補完コマンドを提供する"
  :url "https://github.com/minad/consult"
  :custom ((consult-find-args . "find . -not ( -wholename */.* -prune )"))
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c y" . consult-yasnippet)
         ;; C-x bindings (ctl-x-map)
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
	       ("M-s l" . my-consult-line)
         ("M-s L" . my-consult-line-multi)
         ("M-s r" . my-consult-ripgrep)
	       ("M-s f" . my-consult-fd)
	       ("M-s g r" . my-consult-ghq-ripgrep)
	       ("M-s g f" . my-consult-ghq-find))
  :config
  (leaf consult-flycheck :ensure t
    :doc "Provides the command `consult-flycheck'"
    :url "https://github.com/minad/consult")

  (leaf consult-lsp :ensure t
    :doc "LSP-mode Consult integration"
    :url "https://github.com/gagbo/consult-lsp")

  (leaf consult-yasnippet :ensure t
    :doc "A consulting-read interface for yasnippet"
    :url "https://github.com/mohkale/consult-yasnippet")

  (defun my-consult-line (&optional at-point-symbol)
    "C-uを付けるとthings-at-pointをあらかじめ挿入する consult-line"
    (interactive "P")
    (if at-point-symbol
        (consult-line (thing-at-point 'symbol))
      (consult-line)))

  (defun my-consult-line-multi (&optional at-point-symbol)
    "C-uを付けるとthings-at-pointをあらかじめ挿入する consult-line-multi"
    (interactive "P")
    (if at-point-symbol
        (consult-line-multi nil (thing-at-point 'symbol))
      (consult-line-multi nil)))

  (defun my-consult-ripgrep (&optional at-point-symbol)
    "C-uを付けるとthings-at-pointをあらかじめ挿入する consult-ripgrep"
    (interactive "P")
    (if at-point-symbol
        (consult-ripgrep nil (thing-at-point 'symbol))
      (consult-ripgrep)))

  (leaf my-consult-fd
    :doc "consult-findのfdバージョン"
    :defun (consult--command-split consult--regexp-compiler consult--join-regexps consult--directory-prompt consult--find consult--fd-builder)
    :defvar (consult--regexp-compiler )
    :init
    (defvar consult--fd-command nil)
    (defun consult--fd-builder (input)
      (unless consult--fd-command
        (setq consult--fd-command
              (if (eq 0 (call-process-shell-command "fdfind"))
                  "fdfind"
                "fd")))
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended t)))
        (when re
          (cons (append
                 (list consult--fd-command
                       "--color=never" "--full-path" "--hidden" "--type" "file" "--exclude" ".git/*"
                       (consult--join-regexps re 'extended))
                 opts)
                hl))))

    (defun my-consult-fd (&optional dir initial)
      (interactive "P")

      (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                   (default-directory dir))
        paths
        (find-file (consult--find prompt #'consult--fd-builder initial))))
    )

  (leaf my-consult-ghq
    :doc "ghqの結果をconsultに渡す"
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

    (defun my-consult-ghq-find ()
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
	         (corfu-auto-prefix . 1))
  :bind (:corfu-map
	       ("C-f" . corfu-insert))
  :init
  (global-corfu-mode)
  :config
  (leaf corfu-history :require t
    :doc "Sorting by history for Corfu"
    :url "https://github.com/minad/corfu"
    :defvar (savehist-additional-variables)
    :global-minor-mode t
    :config
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (leaf corfu-indexed :require t
    :doc "Select indexed candidates"
    :url "https://github.com/minad/corfu"
    :global-minor-mode t)

  (leaf corfu-popupinfo :require t
    :doc "Candidate information popup for Corfu"
    :url "https://github.com/minad/corfu"
    :global-minor-mode t
    :custom ((corfu-popupinfo-delay . '(1.0 . 1.0))))

  (leaf kind-icon :ensure t
    :doc "Completion kind icons"
    :url "https://github.com/jdtsmith/kind-icon"
    :defvar (corfu-margin-formatters)
    :custom ((kind-icon-default-face . 'corfu-default))
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(leaf embark :ensure t
  :doc "Conveniently act on minibuffer completions"
  :url "https://github.com/oantolin/embark"
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  (leaf embark-consult :ensure t
    :doc "Consult integration for Embark"
    :url "https://github.com/oantolin/embark"
    :require t
    :hook ((embark-collect-mode-hook . consult-preview-at-point-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; プログラミング関連のユーティリティ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf tree-sitter :ensure (t tree-sitter-langs)
  :doc "各言語に上質なシンタックスハイライトを適用する"
  :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
  :defvar (tree-sitter-major-mode-language-alist)
  :require tree-sitter-langs
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode))
  :config
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
     ])
  (global-tree-sitter-mode))

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

(leaf yasnippet :ensure t
  :doc "Yet another snippet extension for Emacs"
  :url "http://github.com/joaotavora/yasnippet"
  :defun (yas-global-mode)
  :config
  (leaf yasnippet-snippets :ensure t
    :doc "Collection of yasnippet snippets"
    :url "https://github.com/AndreaCrotti/yasnippet-snippets")
  (yas-global-mode 1))

(leaf emmet-mode :ensure t
  :doc "Unofficial Emmet's support for emacs"
  :url "https://github.com/smihica/emmet-mode"
  :hook (typescript-tsx-mode-hook web-mode-hook))

(leaf lsp-mode :ensure t
  :doc "LSP mode"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :hook ((lsp-mode-hook . lsp-enable-which-key-integration)
         (lsp-completion-mode-hook . my-lsp-mode-setup-completion)
         ((typescript-mode-hook typescript-tsx-mode-hook) . lsp-deferred))
  :custom ((lsp-keymap-prefix . "C-c l") ; lsp-mode-mapのキーバインド
           (lsp-headerline-breadcrumb-enable . nil) ; ファイルパスのパンクズを無効化
           (lsp-completion-provider . :none) ; corfuを優先する
           ;; (lsp-enable-indentation . nil)
           ;; typescript
           (lsp-javascript-suggest-auto-imports . nil)
           (lsp-javascript-suggest-paths . nil)
           (lsp-typescript-suggest-auto-imports . nil)
           (lsp-typescript-suggest-paths . nil)
           ; (lsp-clients-typescript-server-args . '("--stdio" "--tsserver-log-file" "/dev/stderr")) ; tsファイルを開くと各プロジェクトフォルダに.logフォルダを作成してしまうのをやめていただく https://github.com/emacs-lsp/lsp-mode/issues/1490#issuecomment-625825914
           ;; rust
           (lsp-rust-analyzer-cargo-watch-command . "clippy")
           ;; (lsp-rust-analyzer-proc-macro-enable . t)
           ;; (lsp-rust-analyzer-experimental-proc-attr-macros . t)
           (lsp-rust-analyzer-server-display-inlay-hints . t)
	         )
  :defvar lsp-file-watch-ignored-directories
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.next\\'")

  ;; https://github.com/emacs-lsp/lsp-mode/issues/2681#issuecomment-1214902146
  (advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (while (re-search-forward "\\u0000" nil t)
                  (replace-match ""))
                (apply orig rest)))

  ;; https://github.com/minad/corfu/wiki#advanced-example-configuration-with-orderless
  (defun my-lsp-mode-setup-completion ()
    "lsp-modeのcapfをcorfuに対応させる"
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (leaf lsp-tailwindcss :ensure t
    :doc "A lsp-mode client for tailwindcss"
    :url "https://github.com/merrickluo/lsp-tailwindcss"
    :custom ((lsp-tailwindcss-add-on-mode . t)))

  (leaf lsp-ui :ensure t
    :doc "UI modules for lsp-mode"
    :url "https://github.com/emacs-lsp/lsp-ui"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; メジャーモード
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf typescript-mode :ensure t
  :doc "Major mode for editing typescript"
  :url "http://github.com/ananthakumaran/typescript.el"
  :preface
  (define-derived-mode typescript-tsx-mode typescript-mode "React")
  :mode ("\\.ts\\'"
         ("\\.tsx\\'" . typescript-tsx-mode))
  :custom ((typescript-indent-level . 2)))

(leaf rustic :ensure t
  :doc "Rust development environment"
  :url "https://github.com/brotzeit/rustic"
  :custom ((rustic-format-trigger . 'on-save)))

(leaf web-mode :ensure t
  :doc "major mode for editing web templates"
  :url "https://web-mode.org"
  :mode ("\\.html\\'" "\\.vue\\'" "\\.php\\'" "\\.xml\\'")
  :custom ((web-mode-attr-indent-offset . nil)
	         (web-mode-markup-indent-offset . 2)
	         (web-mode-css-indent-offset . 2)
	         (web-mode-code-indent-offset . 2)
	         (web-mode-sql-indent-offset . 2)
	         (indent-tabs-mode . nil)
	         (tab-width . 2)
	         (web-mode-script-padding . 2)
	         (web-mode-style-padding . 2)
	         (web-mode-block-padding . 2)
	         (web-mode-enable-current-element-highlight . t)
	         (web-mode-enable-current-column-highlight . t)
	         ;; (web-mode-enable-auto-closing . t)
	         ;; (web-mode-enable-auto-expanding . t)
	         (Web-mode-comment-style . 2)))

(leaf fish-mode :ensure t
  :doc "Fishモード"
  :hook `((fish-mode-hook . ,(lambda ()
				                       (add-hook 'before-save-hook 'fish_indent-before-save)))))

(leaf yaml-mode :ensure t
  :doc "Major mode for editing YAML files"
  :url "https://github.com/yoshiki/yaml-mode"
  :bind (:yaml-mode-map
	       ("C-m" . newline-and-indent)))

(leaf json-mode :ensure t
  :doc "Major mode for editing JSON files."
  :url "https://github.com/joshwnj/json-mode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vtermは最後に書いた方が良い
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf vterm :ensure t
  :doc "Fully-featured terminal emulator"
  :url "https://github.com/akermu/emacs-libvterm"
  :bind ((:vterm-mode-map
          ("C-t" . hydra-window/body))))


;;; init.el ends here
