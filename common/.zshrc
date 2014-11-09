# .zshrc 1.2   Time-stamp: <2014-11-08 22:20:44 PST xoddf2>

zstyle :compinstall filename '~/.zshrc'

# Report background job status immediately
setopt notify

# Do not cd if a command does not exist, or beep on error
unsetopt autocd beep

# Auto-completion
autoload -Uz compinit
compinit

# Emacs keybindings
bindkey -e

# What is a word?
export WORDCHARS="*?.[]~=&;\!#\$%^(){}<>"

# Prompt
autoload -U colors && colors

# Different prompt colour per box
case "$(hostname)" in
	"rofldell") local prompt_color="green" ;;
	"tesseract") local prompt_color="magenta" ;;
	"archon") local prompt_color="yellow" ;;
	"elisabeth") local prompt_color="cyan" ;;
	*) local prompt_color="default" ;;
esac

local cmdstatus="%(?,%{$fg_bold[$prompt_color]%}%#%{$reset_color%},%{$fg_bold[red]%}%#%{$reset_color%})"
export PS1="%{$fg_bold[${prompt_color}]%}%c%{$reset_color%} ${cmdstatus} "

# Make exiting difficult in tmux
if [[ -n "$TMUX" ]] && [[ "$TMUX_PANE" == "%0" ]]; then
	alias exit="false"
	setopt ignoreeof
fi

# Editor
export EDITOR="ed"
if [ -x "$(which emacs)" ]; then
	export VISUAL="emacsclient"
else
	export VISUAL="vi"
fi

# Aliases
if [[ "$(uname -o)" =~ "GNU" ]]; then
	alias ls="ls --color=auto"
	alias grep="grep --color=auto"
fi

if [[ "$(uname)" == "Linux" ]]; then
	alias adx="ps awwfux"
	alias uC="ps wwu -C"
elif [[ "$(uname)" == "FreeBSD" ]]; then
	alias adx="ps adx"
fi

alias ta="tmux attach-session"
alias tad="tmux attach-session -d"
alias tl="tmux list-sessions"
alias lh="ls -lh"
alias lah="ls -lah"
alias aux="ps auxww"
alias p3="ping -c 3"

alias sprunge="curl -F sprunge=@- http://sprunge.us/"
alias ix="curl -n -F 'f:1=<-' http://ix.io"

alias mp="mpv"
alias mp-monaural="mpv --af=pan=1:[0.5,0.5]"
alias mp-overscan="mpv --vf=scale=320:240,crop=288:216 --video-aspect=4:3"
