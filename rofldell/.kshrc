# woddfellow2's ksh Config
# by woddfellow2 | http://wlair.us.to/

# Prompt
export PS1="\e[1;33m\w\e[0m \e[1;34m\$\e[0m "

# Editor
export EDITOR="ed"
export VISUAL="emacsclient -nw --alternate-editor=''"
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
alias ix="curl -n -F 'f:1=<-' http://ix.io"
alias hhbackup="rsync -avz -e ssh --delete tesseract.hackerhaven.net:/home/woddf2/ ~/backup/tesseract/"
alias wlbackup="rsync -avz -e ssh --delete tesseract.hackerhaven.net:/srv/vhosts/wlair.us.to/public/ ~/backup/wlair/"
alias elisabethbackup="rsync -avz -e ssh --delete is.not.a.hax0r.se:/home/xoddf2/ ~/backup/elisabeth/"
alias archonbackup="rsync -avz -e ssh --delete turtil.net:/home/xoddf2/ ~/backup/archon/"
alias wlbackup-turtil="rsync -avz -e ssh --delete turtil.net:/srv/vhosts/wlair.us.to/public/ ~/backup/wlair-turtil/"
