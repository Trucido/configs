PS1="\[\033[01;32m\]\w\[\033[01;34m\] \$\[\033[00m\] "

set -o vi

export EDITOR="vim"

export IGNOREEOF=9001

if [[ -n "$STY" ]] && [[ "$WINDOW" == 0 ]]; then
	alias exit="false"
fi

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias c="clear"
alias mp="mplayer"
