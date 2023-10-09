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
