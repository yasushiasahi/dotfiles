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

# emacs vtermの設定 https://github.com/akermu/emacs-libvterm#shell-side-configuration
function vterm_printf
    if begin
            [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
        end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

if [ "$INSIDE_EMACS" = vterm ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end
end


if status is-interactive
    # Commands to run in interactive sessions can go here

    # 起動時の挨拶文を非表示にする
    set fish_greeting
end
