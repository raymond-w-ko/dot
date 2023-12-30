export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_PAPER=en_US.UTF-8
export LC_NAME=en_US.UTF-8
export LC_ADDRESS=en_US.UTF-8
export LC_TELEPHONE=en_US.UTF-8
export LC_MEASUREMENT=en_US.UTF-8
export LC_IDENTIFICATION=en_US.UTF-8

# ensure a consistent environment
export IFS=' \t\n'
source /etc/profile

## tramp gets hung up on precmd(), unset some features
if [[ "$TERM" == "dumb" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    # unfunction precmd
    # unfunction preexec
    export PATH=$HOME/npm-global/bin:$HOME/go/bin:$HOME/bin:$HOME/dot/bin:$PATH
    PS1='$ '
    return
fi

if [[ ! -f "$XDG_RUNTIME_DIR/ssh-agent.env" ]]; then
  # echo "attempting to kill existing ssh-agent"
  pkill -u "$USER" ssh-agent
fi

if [[ ! -z "$XDG_RUNTIME_DIR" ]]; then
  if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    # echo "starting ssh-agent"
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
  fi
  # echo "importing ssh-agent vars"
  eval "$(<"$XDG_RUNTIME_DIR/ssh-agent.env")" &> /dev/null
else
  echo "no XDG_RUNTIME_DIR"
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
if [[ ! -e "$ZSH/custom/themes/powerlevel10k" ]]; then
  $(cd $ZSH/custom/themes/)
  $(cd $ZSH/custom/themes/ && rm -f powerlevel10k && ln -s $HOME/dot/powerlevel10k powerlevel10k)
fi
ZSH_THEME="powerlevel10k/powerlevel10k"

CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"
zstyle ':omz:update' mode disabled
DISABLE_LS_COLORS="true"
DISABLE_AUTO_TITLE="false"
ENABLE_CORRECTION="false"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="yyyy-mm-dd"

plugins=(git history-substring-search)
if [[ $(uname -s) != MINGW64* ]]; then
  plugins=(${plugins} adb ant)
  plugins=(${plugins} gitfast svn svn-fast-info mercurial lein pip tmux vim-interaction cp)
  plugins=(${plugins} pip brew systemd node npm archlinux aws debian chucknorris screen)
fi

source $ZSH/oh-my-zsh.sh
alias omzr="omz reload"

setopt AUTO_CD
setopt no_sharehistory

# bind UP and DOWN arrow keys
zmodload zsh/terminfo
typeset -g HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=green,fg=black'
typeset -g HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=black'
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

# bind UP and DOWN arrow keys (compatibility fallback
# for Ubuntu 12.04, Fedora 21, and MacOSX 10.9 users)
#bindkey '^[[A' history-substring-search-up
#bindkey '^[[B' history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
source $HOME/dot/src/zsh-z/zsh-z.plugin.zsh
alias c=z
source $HOME/.bashrc

#autoload -U compinit; compinit
zstyle ':completion:*' menu select

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
