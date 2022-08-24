;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yasushi Asahi

;; Author: Yasushi Asahi <asahi1600@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start writing settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cus-edit
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf basic-keybindings
  :init
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (leaf crux
    :ensure t)
  :bind (("C-u")
	 ("C-t")
	 ("C-q")
	 ("C-\\")
	 ("C-m" . newline-and-indent)
	 ("M-/" . help-command)
	 ("C-^" . universal-argument)
	 ("C-M-d" . kill-word)
	 ("C-c l" . toggle-truncate-lines)
	 ("<f5>" . leaf-convert-insert-template)
	 ("<f6>" . leaf-convert-region-replace)
         ("C-u" . crux-smart-open-line-above)
	 ("C-j" . crux-smart-open-line)
	 ("M-k" . crux-kill-whole-line)
	 ("M-h" . crux-kill-line-backwards)
	 ("C-a" . crux-move-beginning-of-line)
	 ("M-d" . crux-duplicate-current-line-or-region)
	 ("M-\\" . crux-duplicate-and-comment-current-line-or-region)))

(leaf basic-settings
  :custom ((truncate-lines . t)                  ; do not wrap end of sentence
           (inhibit-startup-screen . t)          ; do not show startup screeen
           (scroll-preserve-screen-position . t) ; keep cursor position when scrolling
           (scroll-conservatively . 1)           ;
           (initial-scratch-message . "")        ; show nothing in scratch when statrup
           (visible-bell . t)                    ; show bell altanative of beep sound
           (auto-save-timeout . 15)
           (auto-save-interval . 60)
           (version-control . t)
           (create-lockfiles . nil) ; donot make .#xxx file when editing
           (delete-old-versions . t))
  :hook ((before-save-hook . delete-trailing-whitespace)) ; delete trailing whitespace when save
  :preface
  (defalias 'yes-or-no-p 'y-or-n-p)     ; reduce typing, yas -> y, no -> n

  (leaf delsel
    :doc "overwrite region when yank or type"
    :global-minor-mode delete-selection-mode)

  (leaf paren
    :custom ((show-paren-delay . 0)
             (show-paren-style . 'mixed))
    :global-minor-mode show-paren-mode)

  (leaf language
    :preface
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-keyboard-coding-system 'utf-8))

  (leaf files
    :custom `((auto-save-timeout . 15)
              (auto-save-interval . 60)
              (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backups/") t)))
              (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backups"))
                                          (,tramp-file-name-regexp . nil)))
              (version-control . t)
              (delete-old-versions . t)))
  (leaf startup
    :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backups/.saves-"))))
  )

(leaf solarized-theme
  :ensure t
  :custom ((x-underline-at-descent-line . t)
           (solarized-emphasize-indicators . nil))
  :config
  (load-theme 'solarized-dark-high-contrast t)
  (set-cursor-color "#03e635"))

(leaf ns-auto-titlebar
  :ensure t
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(leaf mini-modeline
  :ensure t
  :global-minor-mode t)

(leaf smart-mode-line
    :ensure t
    :custom ((rm-whitelist . '("lsp")))
    :defun (sml/setup)
    :config
    (sml/setup))

(leaf window-customize
  :preface
  (defun split-window-horizontally-n (num_wins)
    "任意の数だけ横に分割"
    (interactive "p")
    (let ((n 1))
      (while (< n num_wins)
        (split-window-horizontally)
        (setq n (1+ n))))
    (balance-windows))
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
    ("0" delete-window)
    ("1" delete-other-windows))
  :defun (split-window-horizontally-n)
  :bind (("C-t" . hydra-window/body)))

(leaf multiple-cursors
  :ensure t
  :bind (("C-q" . hydra-multiple-cursors/body))
  :config
  (with-no-warnings
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
    ("q" nil))))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf smartparens
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode)

(leaf expand-region
  :ensure t
  :bind (("C-o" . er/expand-region)))

(leaf which-key
  :ensure t
  :global-minor-mode t)

(leaf open-junk-file
  :ensure t
  :defvar (my-open-junk-file-format)
  :preface
  (setq my-open-junk-file-format (format
                                  "%s%s"
                                  (string-trim (shell-command-to-string "ghq root"))
                                  "/github.com/yasushiasahi/junkfiles/%Y/%m/%d-%H%M%S."))
  :bind (("C-c j" . open-junk-file))
  :custom ((open-junk-file-find-file-function . 'find-file)
           (open-junk-file-format . my-open-junk-file-format)))


(leaf minibuffer-completion
  :config
  ;; (fido-vertical-mode +1)

  (leaf vertico
    :doc "VERTical Interactive COmpletion"
    :req "emacs-27.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/vertico"
    :added "2022-08-24"
    :emacs>= 27.1
    :ensure t
    :global-minor-mode t)

  (leaf consult
    :doc "Consulting completing-read"
    :req "emacs-27.1" "compat-28.1"
    :tag "emacs>=27.1"
    :url "https://github.com/minad/consult"
    :added "2022-08-24"
    :emacs>= 27.1
    :ensure t
    :bind (("C-s" . consult-line)
           ("C-x b" . consult-buffer)
           )

    )

  (leaf orderless
    :doc "Completion style for matching regexps in any order"
    :req "emacs-26.1"
    :tag "extensions" "emacs>=26.1"
    :url "https://github.com/oantolin/orderless"
    :added "2022-08-24"
    :emacs>= 26.1
    :ensure t
    :custom ((completion-styles . '(orderless basic))))

  )




(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode)


(leaf flycheck
  :ensure t
  :config
  (leaf flycheck-package
    :doc "A Flycheck checker for elisp package authors"
    :ensure t
    :config
    (flycheck-package-setup)
    (global-flycheck-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end writing settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
