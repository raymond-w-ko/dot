if test "$TERM" = "dumb"
    exec sh
end

set fish_greeting

function maybe_add_global_path
    if test -d $argv
        fish_add_path -g $argv
    end
end

maybe_add_global_path /c/Users/$USER/.cargo/bin

source $HOME/dot/fisher/functions/fisher.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    #eval (dircolors -c ~/dot/src/dircolors-solarized/dircolors.ansi-universal)
end

abbr -a -- g 'git'
abbr -a -- gcfxd 'git clean -fxd'
abbr -a -- gsw 'git switch'
abbr -a -- gp 'git push'
abbr -a -- gs 'git status'
abbr -a -- gd 'git diff'
abbr -a -- gc 'git commit'
abbr -a -- gca 'git commit -a'
abbr -a -- ga 'git add'
abbr -a -- gl 'git pull'

abbr -a -- c 'z'
abbr -a -- dot 'cd ~/dot'
abbr -a -- y 'yay'
abbr -a -- adu 'sudo apt dist-upgrade'

abbr -a -- ta 'tmux attach -d -t'
abbr -a -- ts 'tmux new -s'
abbr -a -- tl 'tmux list-sessions'
abbr -a -- us 'update-submodules'

abbr -a -- .. 'cd ..'
abbr -a -- ... 'cd ../..'
abbr -a -- .... 'cd ../../..'

if command -q eza
    abbr -a -- l 'eza -l'
    abbr -a -- e 'eza -l'

    abbr -a -- ll 'eza -la'
    abbr -a -- ee 'eza -la'
    abbr -a -- ea 'eza -la'
else
    abbr -a -- l 'ls -l'
    abbr -a -- e 'ls -l'
end
