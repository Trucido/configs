# .zshrc 1.1   Time-stamp: <2014-02-05 00:59:12 xoddf2>

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

# Prompt
autoload -U colors && colors

# Different prompt colour per box
case "$(hostname)" in
	"rofldell") local prompt_color="green" ;;
	"tesseract") local prompt_color="magenta" ;;
	"elisabeth") local prompt_color="cyan" ;;
	"archon") local prompt_color="blue" ;;
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

alias p3="ping -c 3"
alias sprunge="curl -F sprunge=@- http://sprunge.us/"
alias ix="curl -n -F 'f:1=<-' http://ix.io"

alias mp="mplayer"
alias mp-overscan="mplayer -vf scale=320:240,crop=288:216 -aspect 4:3"
alias mp-monaural="mplayer -af pan=1:0.5:0.5"

if [[ $(hostname) == "oldbox" ]]; then
	alias tuneraudio="aplay -f dat < /dev/video24"
	alias tunervideo="mplayer /dev/video32 -demuxer rawvideo -rawvideo w=720:h=480:format=hm12:ntsc"
fi
