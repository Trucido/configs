# $Id: .zshrc,v 1.0.1 2013/08/23 19:34:08 xoddf2 Exp $

zstyle :compinstall filename '~/.zshrc'

# Report background job status immediately
setopt notify

# Do not cd if a command does not exist, or beep on error
unsetopt autocd beep

# Auto-completion
autoload -Uz compinit
compinit

# Prompt	
autoload -U colors && colors
local cmdstatus="%(?,%{$fg_bold[blue]%}%#%{$reset_color%},%{$fg_bold[red]%}%#%{$reset_color%})"
export PS1="%{$fg_bold[green]%}%c%{$reset_color%} ${cmdstatus} "

# Emacs Keybindings
bindkey -e

# Make Exiting Difficult
if [[ -n "$STY" ]] && [[ "$WINDOW" == "0" ]]; then
	alias exit="false"
	setopt ignoreeof
fi

# Default Applications
export EDITOR="emacsclient --alternate-editor='' -nw"

# Aliases
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias p3="ping -c 3"
alias mp="mplayer"
alias sprunge="curl -F sprunge=@- http://sprunge.us/"
