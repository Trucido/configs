# .zshrc 1.4.10
# Time-stamp: <2016-04-30 04:29:52 PDT xoddf2>

zstyle :compinstall filename '~/.zshrc'

# Misc settings
setopt notify
setopt correct

unsetopt autocd beep

# Auto-completion
autoload -Uz compinit
compinit

# Keybindings
bindkey -e

# What is a word?
export WORDCHARS="*?.[]~=&;\!#\$%^(){}<>"

# Prompt
autoload -U colors && colors

# Different prompt colour per box
case "$(hostname)" in
	"nomad") local prompt_color="green" ;;
	"archon") local prompt_color="yellow" ;;
	"elisabeth") local prompt_color="cyan" ;;
	*) local prompt_color="default" ;;
esac

local cmdstatus="%(?,%{$fg_bold[$prompt_color]%}%#%{$reset_color%},%{$fg_bold[red]%}%? %#%{$reset_color%})"
export PS1="%{$fg_bold[${prompt_color}]%}%~%{$reset_color%} ${cmdstatus} "
export RPROMPT="%D{%Y-%m-%d} %*"

# History
HISTFILE=~/.zhistory
HISTSIZE=10000
SAVEHIST=10000

setopt HIST_IGNORE_DUPS SHARE_HISTORY

# Avoid accidentally exiting main shell in tmux
if [[ -n "$TMUX" ]] && [[ "$TMUX_PANE" == "%0" ]]; then
	alias exit="false"
	alias logout="false"
	setopt ignoreeof
fi

# ...or in an Emacs "M-x shell" buffer
if [[ -n "$INSIDE_EMACS" ]]; then
	alias exit="false"
	alias logout="false"
	setopt ignoreeof

	# To avoid glitchiness:
	unsetopt zle
fi

# Editor
export EDITOR="ed"
[ -x "$(which emacs)" ] && export VISUAL="emacsclient" || export VISUAL="vi"

# Aliases
if [[ "$(uname -o)" =~ "GNU" ]]; then
	alias ls="ls --color=auto"
	alias grep="grep --color=auto"
fi

case "$(uname)" in
	"FreeBSD")
		alias adx="ps adx" ;;
	"Linux")
		alias adx="ps awwfux"
		alias uC="ps wwu -C"
		;;
esac

alias ta="tmux attach-session"
alias tad="tmux attach-session -d"
alias tl="tmux list-sessions"
alias la="ls -a"
alias lh="ls -lh"
alias lah="ls -lah"
alias aux="ps auxww"
alias p3="ping -c 3"

alias sprunge="curl -F sprunge=@- http://sprunge.us/"
alias ix="curl -n -F 'f:1=<-' http://ix.io"

alias mp="mpv"
alias mp-monaural="mpv '--af=pan=1:[0.5,0.5]'"
alias mp-overscan="mpv --vf=scale=320:240,crop=288:216 --video-aspect=4:3"
