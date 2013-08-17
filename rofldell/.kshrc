# woddfellow2's ksh Config
# by woddfellow2 | http://wlair.us.to/

# Prompt
export PS1="\w \$ "

# Editor
set -o vi
export EDITOR="vi"
export FCEDIT="$EDITOR"

# Make exiting difficult in tmux
if [[ -n "$TMUX" ]] && [[ "$TMUX_PANE" == "%0" ]]; then
	alias exit="false"
	set -o ignoreeof
fi

# Aliases
alias mp="mplayer"
alias p3="ping -c 3"
alias sprunge="curl -F sprunge=@- http://sprunge.us/"
alias hhbackup="rsync -avz -e ssh --delete tesseract.hackerhaven.net:/home/woddf2/ ~/backup/tesseract/"
alias wlbackup="rsync -avz -e ssh --delete tesseract.hackerhaven.net:/srv/vhosts/wlair.us.to/public/ ~/backup/wlair/"
