if test "$TERM" = "dumb"
    exec sh
end

set -gx COLORTERM "truecolor"

set fish_greeting

function maybe_add_global_path
    if test -d $argv
        # this adds to the beginning of the path, and uses global $fish_user_paths
        fish_add_path -g $argv
    end
end

maybe_add_global_path /bin
maybe_add_global_path /sbin
maybe_add_global_path /usr/sbin
maybe_add_global_path /usr/bin
maybe_add_global_path /opt/homebrew/bin
maybe_add_global_path /usr/local/sbin
maybe_add_global_path /usr/local/bin
if test -d /usr/lib/ccache/bin
    maybe_add_global_path /usr/lib/ccache/bin
else
    maybe_add_global_path /usr/lib/ccache
end
maybe_add_global_path /usr/local/go/bin
maybe_add_global_path /opt/mono/bin
maybe_add_global_path /opt/aws/bin
maybe_add_global_path /opt/tastyworks

maybe_add_global_path /opt/homebrew/opt/node@22/bin
set -gx LDFLAGS "-L/opt/homebrew/opt/node@22/lib"
set -gx CPPFLAGS "-I/opt/homebrew/opt/node@22/include"

maybe_add_global_path /opt/homebrew/opt/openjdk@21/bin
set -gx CPPFLAGS "-I/opt/homebrew/opt/openjdk@21/include"

maybe_add_global_path $HOME/.config/yarn/global/node_modules/.bin
maybe_add_global_path $HOME/npm-global/bin
maybe_add_global_path $HOME/.nimble/bin

maybe_add_global_path $HOME/src/neil
maybe_add_global_path $HOME/.local/bin
maybe_add_global_path $HOME/vim/bin
maybe_add_global_path $HOME/nvim/bin
maybe_add_global_path $HOME/emacs/bin

maybe_add_global_path $HOME/go/bin
set -gx GOPATH "$HOME/gopath"
mkdir -p $GOPATH
maybe_add_global_path $HOME/gopath/bin

# bun
set --export BUN_INSTALL "$HOME/.bun"
# set --export PATH $BUN_INSTALL/bin $PATH
maybe_add_global_path $BUN_INSTALL/bin

# zig (for ghostty)
maybe_add_global_path $HOME/zig-x86_64-linux-0.14.1
maybe_add_global_path $HOME/zig-aarch64-linux-0.14.1
maybe_add_global_path $HOME/zig-x86_64-linux-0.15.1
maybe_add_global_path $HOME/zig-aarch64-linux-0.15.1

maybe_add_global_path $HOME/dot/bin
maybe_add_global_path $HOME/bin

if command -q zoxide
  zoxide init fish --cmd j | source
end

source $HOME/dot/fisher/functions/fisher.fish

if test -f $HOME/dot/config/fish/local.config.fish
  source $HOME/dot/config/fish/local.config.fish
end

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
abbr -a -- gf 'git fetch'
abbr -a -- gfa 'git fetch --all'

abbr -a -- c 'z'
abbr -a -- dot 'cd ~/dot'
abbr -a -- y 'yay'
abbr -a -- adu 'sudo apt update && sudo apt dist-upgrade'
abbr -a -- afu 'sudo apt update && sudo apt full-upgrade'

abbr -a -- ta 'tmux attach -d -t'
abbr -a -- ts 'tmux new -s'
abbr -a -- tl 'tmux list-sessions'
abbr -a -- us 'update-submodules'

abbr -a -- oc 'opencode'
abbr -a -- cdx 'codex'

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
  abbr -a -- l 'ls -l --color=auto'
  abbr -a -- e 'ls -l --color=auto'
end

if command -q nvim
  abbr -a -- v 'nvim'
  abbr -a -- vim 'nvim'
  set -gx EDITOR nvim
else if command -q vim
  abbr -a -- v 'vim'
  set -gx EDITOR vim
else
  abbr -a -- v 'nano'
  set -gx EDITOR nano
end
