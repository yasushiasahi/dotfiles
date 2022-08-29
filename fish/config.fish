# homebrewのの初期化
eval (/opt/homebrew/bin/brew shellenv)

# asdfの初期化
source /opt/homebrew/opt/asdf/libexec/asdf.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
end
