# these are the main aliases if you are using a sh compatible shell like bash or zsh

unameString=`uname -s`

if [[ "$unameString" == 'Darwin' ]]; then
    # if hash gdircolors 2>/dev/null; then
    #   if [ -r "$HOME/src/dircolors-solarized/dircolors.256dark" ]; then
    #     eval `gdircolors $HOME/src/dircolors-solarized/dircolors.256dark`
    #   fi
    # fi

    if hash gls 2>/dev/null; then
      alias ls='gls -F'
      alias l='gls -lhF'
      alias lt='gls -lhtF'
      alias ll='gls -lhaF'
    else
      # export LSCOLORS=ExFxCxDxBxegedabagacad
      alias ls='ls -G'
      alias l='ls -Glh'
      alias lt='ls -Glht'
      alias ll='ls -Glha'
    fi
else
    if hash eza 2>/dev/null; then
      alias ls='eza --git-ignore'
      alias la='eza -la'
      alias ll='eza -la'
      alias l='eza --git-ignore -l'
      alias lt='eza --git-ignore -l --sort time'
      alias lsi='eza --git-ignore -l --sort size'
    elif hash exa 2>/dev/null; then
      alias ls='exa'
      alias la='exa -la'
      alias ll='exa -la'
      alias l='exa -l'
      alias lt='exa -l --sort time'
      alias lsi='exa -l --sort size'
    else
      alias ls='ls --color=auto -F'
      alias l='ls --color=auto -lhF'
      alias lt='ls --color=auto -lhtF'
      alias ll='ls --color=auto -lhaF'
    fi

    export GREP_COLORS='mt=1;31'

    # fix ls colors especially for directories and files that are globally
    # readable and writeable, they are completely unreadable by default
    # if [ -r "$HOME/src/dircolors-solarized/dircolors.ansi-light" ]; then
    #   eval `dircolors $HOME/src/dircolors-solarized/dircolors.ansi-universal`
    #   export LS_COLORS="$LS_COLORS:ow=1;7;34:st=30;44:su=30;41"
    # else
    #   eval `dircolors -b`
    # fi
fi

if [[ -d $HOME/nvim ]]; then
  alias v=nvim
  alias vs='nvim -S'
elif hash nvim 2>/dev/null; then
  alias v=nvim
  alias vs='nvim -S'
else
  alias v=vim
  alias vs='vim -S'
fi
alias ec="emacsclient"
alias rmr="rm -r"
alias rmri="rm -r -i"
alias r="ranger"
alias sr="sudo ranger"

alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'

if [[ -f "/usr/bin/backblaze-b2" ]]; then
  alias bbl=/usr/bin/backblaze-b2
elif [[ -f "$HOME/.local/bin/b2" ]]; then
  alias bbl="$HOME/.local/bin/b2"
elif [[ -f "/usr/local/bin/b2" ]]; then
  alias bbl="/usr/local/bin/b2"
fi

hash schedtool 2>/dev/null
if [[ $? == 0 && -f /proc/sys/kernel/rr_interval ]]; then
	NICE_PROG="schedtool -D -e"
else
  NICE_PROG="nice -n 19"
fi

alias dot="cd $HOME/dot"
alias SS="sudo systemctl"
alias SSu="systemctl --user"
alias sdr='screen -U -D -R'
# covered by oh-my-zsh, but I do not want to type argument
# alias ta='tmux attach-session -d'
alias psref="gpg-connect-agent RELOADAGENT /bye" # refresh gpg
alias genctags='/usr/bin/find . -regex ".*\.\(c\|h\|hpp\|cc\|cpp\)" -print | /usr/bin/ctags --c++-kinds=+px --fields=+aimSz --languages=c++ --sort=yes -L -'
alias killpngcolorpofile='find . -type f -name "*.png" -exec convert {} -strip {} \;'
alias iotop='sudo iotop -oP'
alias iftop='sudo iftop'
alias openports='ss --all --numeric --processes --ipv4 --ipv6'
alias n19='nice -n 19'
alias n="$NICE_PROG"
alias makepkg="$NICE_PROG makepkg"
alias y="$NICE_PROG yay"
alias adu="sudo apt-get update && sudo apt-get dist-upgrade"
alias fun="fusermount -u"
alias npmreinstall="rm -rf node_modules/ && npm install"
alias findrmemacsbackups="find . -type f -name '*~' -exec rm -i {} \;"

alias lg='lazygit'
alias gs='git status' # override ghostscript
# alias gc='git commit -v'
# alias gca='git commit -v -a'
alias gcasubmodules='git commit -v -a -m "updated submodule(s)"'
alias gcsubmodules='git commit -v -m "updated submodule(s)"'
# alias gd='git diff'
# alias gdw='git diff --color-words'
# alias gp='git push'
# alias gl='git pull'
# alias ga='git add'
# alias gaa='git add --all'
# alias gapa='git add --patch'
alias gcfxd='git clean -fxd'
# alias gcfxdrh='git clean -fxd && git reset --hard'
alias gitrmmissing='git ls-files --deleted -z | xargs -0 git rm'
alias gdch="git diff --color-words='\+|.'"

alias svnlog='svn log -l 1024 -v | less'
alias svnfixexe="find . -name '*.sh' -exec svn propset svn:executable yes '{}' \;"
svnrmmissing() {
  svn st | grep ^! | awk '{print " --force "$2}' | xargs svn rm
}
svnaddmissing() {
  svn st | grep '^?' | awk '{print " --force "$2}' | xargs svn add
}
alias svnadddir='svn add --depth=empty'
svndiff() {
  svn diff --diff-cmd colordiff -x "-u -w -p" "$@" | less -R
}
svnshow() {
  svn diff --diff-cmd colordiff -x "-u -w -p" -c $1 | less -R
}
alias svnignore='svn propedit svn:ignore'

# docker LOL
dockerclean() {
  docker rm -v $(docker ps --filter status=exited -q 2>/dev/null) 2>/dev/null
  docker rmi $(docker images --filter dangling=true -q 2>/dev/null) 2>/dev/null
}
dockercleanstoppedimages() {
  docker ps -aq --no-trunc | xargs docker rm
}

idleionice() {
  ionice -c3 -p $@
}
if [[ -x $(command -v fzf) ]]; then
  alias f="fzf"
fi
export BAT_THEME=gruvbox-light
if [[ -x $(command -v fdfind) ]]; then
  export FZF_DEFAULT_OPTS="--no-bold"
  export FZF_DEFAULT_COMMAND='fdfind --type f --hidden --exclude .git --exclude .fzf --exclude .svn'
elif [[ -x $(command -v fd) ]]; then
  export FZF_DEFAULT_OPTS="--no-bold"
  export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git --exclude .fzf --exclude .svn'
fi

findcore() {
  find . -type f -regextype posix-extended -regex '.*/core\.[0-9]+$'
}
alias us='update-submodules'
alias rg="rg -i --colors 'match:bg:yellow' --colors 'match:fg:black' --colors 'match:style:nobold' --colors 'line:fg:yellow' --colors 'path:fg:green'"
alias fix_permissions="find . -regex '.*\.\(vim\|h\|hpp\|c\|cpp\)$' -type f -exec chmod -x {} \;"
superwget() {
  while [ 1 ]; do
    wget --retry-connrefused --waitretry=1 --read-timeout=20 --timeout=15 -t 0 --continue "$1"
    if [ $? = 0 ]; then
      break;
    fi; # check return value, break if successful (0)
    sleep 1s;
  done;
}
