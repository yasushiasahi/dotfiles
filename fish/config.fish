# homebrewのの初期化
eval (/opt/homebrew/bin/brew shellenv)

# asdfの初期化
source /opt/homebrew/opt/asdf/libexec/asdf.fish

# エイリアス
balias ls 'exa -alh --icons'
balias tree 'exa --tree'
balias rm trash
balias E 'open -a /Applications/Emacs.app/'

if status is-interactive
    # Commands to run in interactive sessions can go here
end
