# woddfellow2's ksh Config
# by woddfellow2 | http://wlair.us.to/

# Prompt
export PS1="\e[1;33m\w\e[0m \e[1;34m\$\e[0m "

# Editor
set -o vi
export EDITOR="vim"
export FCEDIT="$EDITOR"

# Make exiting difficult in tmux
if [[ -n "$TMUX" ]] && [[ "$TMUX_PANE" == "%0" ]]; then
	alias exit="false"
	set -o ignoreeof
fi

# Aliases
alias mp="mplayer"
alias p3="ping -c 3"
