# homebrewのの初期化
eval (/opt/homebrew/bin/brew shellenv)

# asdfの初期化
source /opt/homebrew/opt/asdf/libexec/asdf.fish

# XDG Base Directory の設定: https://wiki.archlinux.org/index.php/XDG_Base_Directory
set -Ux XDG_CONFIG_HOME $HOME/.config
set -Ux XDG_CACHE_HOME $HOME/.cache
set -Ux XDG_DATA_HOME $HOME/.local/share
set -Ux XDG_DATA_DIRS /usr/local/share:/usr/share
set -Ux XDG_CONFIG_DIRS /etc/xdg


# その他環境変数
set -Ux LSP_USE_PLISTS true # emacsのlsp-mode用: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization


# エイリアス
balias ls 'exa -alh --icons'
balias tree 'exa --tree'
balias rm trash
balias E 'open -a /Applications/Emacs.app/'

if status is-interactive
    # Commands to run in interactive sessions can go here

    # 起動時の挨拶文を非表示にする
    set fish_greeting
end
