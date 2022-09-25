# homebrewのの初期化
eval (/opt/homebrew/bin/brew shellenv)

# asdfの初期化
source /opt/homebrew/opt/asdf/libexec/asdf.fish

# Rust関連のPATH
set -U fish_user_paths $fish_user_paths $HOME/.cargo/bin

# XDG Base Directory の設定: https://wiki.archlinux.org/index.php/XDG_Base_Directory
set -Ux XDG_CONFIG_HOME $HOME/.config
set -Ux XDG_CACHE_HOME $HOME/.cache
set -Ux XDG_DATA_HOME $HOME/.local/share
set -Ux XDG_DATA_DIRS /usr/local/share:/usr/share
set -Ux XDG_CONFIG_DIRS /etc/xdg

# emacsのlsp-mode用: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
set -Ux LSP_USE_PLISTS true

# エイリアス
balias ls 'exa -alh --icons'
balias tree 'exa --tree --git-ignore'
balias rm trash
balias E 'open -a /Applications/Emacs.app/'

if status is-interactive
    # Commands to run in interactive sessions can go here

    # 起動時の挨拶文を非表示にする
    set fish_greeting

    # emacs vtermの設定を読み込む https://github.com/akermu/emacs-libvterm#shell-side-configuration-files
    if test "$INSIDE_EMACS" = vterm && test -n $EMACS_VTERM_PATH && test -f {$EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
        source {$EMACS_VTERM_PATH}/etc/emacs-vterm.fish
    end
end
