export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# ensure a consistent environment
if grep -qE "(Microsoft|WSL)" /proc/version &> /dev/null ; then
  true
else
  export PATH=
fi
# Append our default paths
appendpath () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}
appendpath '/usr/local/sbin'
appendpath '/usr/local/bin'
appendpath '/usr/bin'
appendpath '/bin'
unset appendpath

source /etc/profile

if [[ ! -z "$XDG_RUNTIME_DIR" ]]; then
  if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
  fi
  if [[ ! -z "$SSH_AUTH_SOCK" ]]; then
    eval "$(<"$XDG_RUNTIME_DIR/ssh-agent.env")"
  fi
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="lambda"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"
function set_terminal_title() {
  echo -en "\e]2;$@\a"
}
function set-title() {
  echo -en "\e]2;$@\a"
}
function precmd () {
  set_terminal_title $PWD
}

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

# base
plugins=(git gitfast mercurial lein pip svn svn-fast-info tmux screen vim-interaction cp chucknorris history-substring-search debian)
# android development
plugins=(${plugins} adb ant)
if [[ $(uname -s) != CYGWIN* ]]; then
  plugins=(${plugins} pip brew systemd node npm archlinux aws)
fi

# stop eating characters after tab completion
export ZLE_REMOVE_SUFFIX_CHARS=""

# User configuration
fpath=(
  /usr/local/share/zsh-completions
  /usr/local/share/zsh/site-functions
  /usr/share/zsh/site-functions
  $fpath
)
source $ZSH/oh-my-zsh.sh

# oh-my-bug!
# https://github.com/robbyrussell/oh-my-zsh/issues/1398
## case-insensitive (all),partial-word and then substring completion
if [ "x$CASE_SENSITIVE" = "xtrue" ]; then
	zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
else
	if [ "x$HYPHEN_INSENSITIVE" = "xtrue" ]; then
		zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
	else
		zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
	fi
fi

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
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

bindkey -s 'θ' '"\C-b"'
bindkey -s 'ω' "'\C-b'"
bindkey -s 'υ' ">\C-b<"
bindkey -s 'σ' "}\C-b{"
bindkey -s 'φ' ")\C-b("
bindkey -s 'ρ' "]\C-b["

# no TAB completion due to bad chroma 4:2:0 monitor
zstyle ':completion:*' list-colors

ZSH_THEME_GIT_PROMPT_SUFFIX=""
function prompt_rko_setup {
  autoload -U colors && colors
  local NEWLINE=$'\n'
  ps1=(
    "$NEWLINE"
    # user @ host
    "%n at %{$fg_bold[cyan]%}%m "
    # on
    "${reset_color}on "
    # date and time
    "%D{%a, %b %d %Y, %I:%M:%S %p} "
    "$NEWLINE"
    # version control
    '$(git_prompt_info)'
    '$(hg_prompt_info)'
    ' '
    # path
    "%{$fg_bold[yellow]%}%~ "
    # arrow
    "${reset_color}$NEWLINE"
    " > "
  )
  PS1="${(j::)ps1}"
}
prompt_rko_setup
[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
source $HOME/.bashrc
source $HOME/dot/src/zsh-z/zsh-z.plugin.zsh
alias c=z
autoload -U compinit && compinit
zstyle ':completion:*' menu select
