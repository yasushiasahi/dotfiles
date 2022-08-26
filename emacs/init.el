;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

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
  (leaf solarized-theme :ensure t
    :doc "Solarizedテーマを適用"
    :url "http://github.com/bbatsov/solarized-emacs"
    :custom ((solarized-emphasize-indicators . nil)
             (solarized-use-less-bold . t))
    :init
    (load-theme 'solarized-dark t))

  (leaf fira-code-mode :ensure t
    :doc "Fira Codoフォントのリガチャをいい感じに設定"
    :url "https://github.com/jming422/fira-code-mode"
    :custom ((fira-code-mode-disabled-ligatures . '("[]" "#{" "#(" "#_" "#_(" "x")))
    :hook prog-mode-hook)

  ;; https://zenn.dev/hyakt/articles/6ff892c2edbabb#%E8%A8%AD%E5%AE%9A
  (leaf tree-sitter :ensure (t tree-sitter-langs)
    :doc "各言語に上質なシンタックスハイライトを適用する"
    :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
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

  (defun display-startup-echo-area-message ()
    "Emacs起動後エコーエリアに表示される文字列"
    (message "オレ達が本当に力を貸してほしい時にはきっと連絡とれるよ逆にオレ達の力が本当に必要な時はあっちから連絡をくれる by ゴン・フリークス『HUNTER×HUNTER』")))

(leaf 細々した内部的な設定
  :init
  (leaf no-littering :ensure t
    :doc "自動生成される設定ファイルの保存先に秩序をもたらす"
    :url "https://github.com/emacscollective/no-littering"
    :custom `(;; init.el内にcustom-set-variablesのダンプを履かせない
	      (custom-file . ,(no-littering-expand-etc-file-name "custom.el"))
	      ;; オートセーブファイルの場所
	      (auto-save-file-name-transforms . '((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
    :config
    (leaf recentf
      :doc "最近開いたファイルの履歴を保存する"
      :global-minor-mode t
      :custom ((recentf-max-saved-items . 2000))
      :config
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory))))

(leaf 使いやすいキーバインドにするぞ
  :init
  ;; https://www.emacswiki.org/emacs/BackspaceKey
  (define-key key-translation-map [?\C-?] [?\C-h])
  (define-key key-translation-map [?\M-\d] [?\M-h])
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\M-h] [?\M-\d]))

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

  (leaf multiple-cursors :ensure t
    :doc "複数カーソルを操作する"
    :url "https://github.com/magnars/multiple-cursors.el"
    :bind (("C-q" . hydra-multiple-cursors/body))
    :config
    ;; https://github.com/abo-abo/hydra/wiki/multiple-cursors
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
  (leaf ミニバッファ補完を活用するぞ
    :init
    (leaf vertico :ensure t
      :doc "ミニバッファに垂直に候補を表示して、インタラクティブに選択出来るUIを提供する"
      :url "https://github.com/minad/vertico"
      :global-minor-mode t)

    (leaf consult :ensure t
      :doc "補完コマンドを提供する"
      :url "https://github.com/minad/consult"
      :bind (;; C-x bindings (ctl-x-map)
	     ("C-x b" . consult-buffer)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
	     ;; M-g bindings (goto-map)
             ("M-g g" . consult-goto-line)
	     ;; M-s bindings (search-map)
	     ("M-s l" . consult-line)
             ("M-s r" . consult-ripgrep)
	     ("M-s d" . consult-find)
	     ("M-s g r" . my-consult-ghq-ripgrep)
	     ("M-s g d" . my-consult-ghq-find))
      :config
      (leaf my-consult-ghq:ghqの結果をconsultに渡す
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
	  (consult-find (my-consult-ghq--read)))

	(defun my-consult-ghq-ripgrep ()
	  "ghq管理のリポジトリ一覧から選び、プロジェクト内でripgrep"
	  (interactive)
	  (consult-ripgrep (my-consult-ghq--read)))))

    (leaf orderless :ensure t
      :doc "補完候補絞込のいい感じのスタイルを提供する"
      :url "https://github.com/oantolin/orderless"
      :custom ((completion-styles . '(orderless basic))))

    (leaf marginalia :ensure t
      :doc "補完候補の詳細を表示する"
      :url "https://github.com/minad/marginalia"
      :global-minor-mode t))

  (leaf オートコンプリートはコードエディタの真骨頂
    :init
    (leaf corfu :ensure t
      :doc "オートコンプリートインターフェイス"
      :url "https://github.com/minad/corfu"
      :custom ((corfu-auto . t)
	       (corfu-quit-no-match . t)
	       (corfu-auto-prefix . 2))
      :init
      (global-corfu-mode)
      :config
      (leaf corfu-doc :ensure t
	:doc "Corfuの選択対象のドキュメントを表示"
	:url "https://github.com/galeo/corfu-doc"
	:bind (:corfu-map
	       ("M-p" . corfu-doc-scroll-down)
	       ("M-n" . corfu-doc-scroll-up))
	:hook corfu-mode-hook))))


;;; init.el ends here
