set fish_greeting

function maybe_add_global_path
    if test -d $argv
        fish_add_path -g $argv
    end
end

maybe_add_global_path /c/Users/$USER/.cargo/bin

if status is-interactive
    # Commands to run in interactive sessions can go here
    #eval (dircolors -c ~/dot/src/dircolors-solarized/dircolors.ansi-universal)
end

abbr -a -- gp 'git push'
abbr -a -- gs 'git status'
abbr -a -- gd 'git diff'
abbr -a -- gc 'git commit'
abbr -a -- ga 'git add'
abbr -a -- gl 'git pull'
if command -q eza
    abbr -a -- l 'eza -l'
    abbr -a -- e 'eza -l'
    abbr -a -- ea 'eza -l'
else
    abbr -a -- l 'ls -l'
    abbr -a -- e 'ls -l'
end

