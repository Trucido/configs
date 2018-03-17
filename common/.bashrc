#!/usr/bin/env bash
# .bashrc
# Time-stamp: <2018-03-17 01:36:16 PDT xoddf2>

# Check for an interactive shell
if [[ $- != *i* ]]; then
	return
fi

# Settings #####################################################################

# Globbing
shopt -s extglob
shopt -s nocaseglob
shopt -s dotglob

# Other
set -o notify
set bell-style none
shopt -s checkwinsize
shopt -s no_empty_cmd_completion

# Keybindings ##################################################################

# Emacs keybindings
set -o emacs

# Make readline match Emacs comint
bind '"\ep": previous-history'
bind '"\en": next-history'
bind '"\er": reverse-search-history'

# History ######################################################################

HISTSIZE=10000
HISTFILESIZE=10000

shopt -s histappend
shopt -s cmdhist

# Editor #######################################################################

EDITOR="ed"

if command -v emacs >/dev/null 2>&1; then
	VISUAL="emacsclient"
elif command -v zile >/dev/null 2>&1; then
	VISUAL="zile"
elif command -v mg >/dev/null 2>&1; then
	VISUAL="mg"
else
	VISUAL="vi"
fi

export EDITOR VISUAL

# Prompt #######################################################################

set_prompt()
{
	local exit_status='$?'

	# Different prompt colour per box
	case "$HOSTNAME" in
		"nomad") local hostname_color="$(tput bold; tput setaf 3)" ;;
		"salt") local hostname_color="$(tput bold; tput setaf 2)" ;;
		*) local hostname_color="$(tput bold)" ;;
	esac

	# Additional colours
	local prompt_color="$(tput bold; tput setaf 4)"
	local failure_color="$(tput bold; tput setaf 1)"
	local date_color="$(tput setaf 6)"
	local history_color="$(tput bold; tput setaf 2)"

	local reset="$(tput sgr0)"

	PS1="\[$date_color\][\D{%Y-%m-%d %H:%M:%S}]\[$reset\] \[$hostname_color\]\u@\h:\w\[$reset\] \[$failure_color\]\[$exit_status\]\[$reset\] \[$history_color\]\!\[$reset\] \[$prompt_color\]\$\[$reset\] "
}

PROMPT_COMMAND="history -a; history -n; set_prompt"

# Aliases ######################################################################

# Avoid accidentally exiting main shell in tmux or Emacs shell-mode
if [ -n "$TMUX" ] && [ "$TMUX_PANE" = "%0" ] || [ -n "$INSIDE_EMACS" ]; then
	alias exit="false"
	alias logout="false"
	set -o ignoreeof
fi

# ls and grep
if [[ $OSTYPE =~ gnu ]]; then
	alias ls="ls --color=auto -lh --group-directories-first"
	alias la="\ls --color=auto -lah --group-directories-first"
	alias grep="grep --color=auto"
else
	alias ls="ls -lh"
	alias la="\ls -lah"
fi

# ps
alias ps="ps auxww"

if [ "$OSTYPE" = "linux-gnu" ]; then
	alias psr="\ps auxww --sort=%cpu"
	alias psm="\ps auxww --sort=rss"
	alias psx="\ps awwfux"
	alias psc="\ps wwu -C"
fi

alias df="df -hT"

alias p3="ping -c 3"

# tmux
if command -v tmux >/dev/null 2>&1; then
	alias ta="tmux attach-session"
	alias tad="tmux attach-session -d"
	alias tl="tmux list-sessions"
fi

# Disk mounting
if command -v udisksctl >/dev/null 2>&1; then
	alias udmount="udisksctl mount -b"
	alias udumount="udisksctl unmount -b"
fi

# Git
if command -v git >/dev/null 2>&1; then
	alias gaa="git add -A"
	alias gcm="git commit -a -m"
	alias gpm="git push origin master"
fi

# Battery information
if [ "$HOSTNAME" = "nomad" ]; then
	alias bat="acpi -V"
fi

# Pastebins
alias sprunge="curl -F sprunge=@- http://sprunge.us/"
alias ix="curl -n -F 'f:1=<-' http://ix.io"

# mpv (Long ago, this pointed to MPlayer.)
if command -v git >/dev/null 2>&1; then
	alias mp="mpv"
	alias mp-monaural="mpv '--af=pan=1:[0.5,0.5]'"
	alias mp-overscan="mpv --vf=scale=320:240,crop=288:216 --video-aspect=4:3"
fi

# Compare to "eshell-visual-commands"
if [ -n "$INSIDE_EMACS" ]; then
	alias less="tmux new-window less"
	alias htop="tmux new-window htop"
	alias sl="ls"
fi
