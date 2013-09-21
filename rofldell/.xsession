# woddfellow2's ~/.xsession (rofldell)
# by woddfellow2 | http://wlair.us.to/

export LC_CTYPE="en_US.UTF-8"

xscreensaver -no-splash &
setxkbmap -option "caps:escape" -option "compose:ralt" &

xsetroot -solid rgb:00/00/33 &

xclock -geometry -0+0 &
xload -geometry -0+40 &
xbiff -geometry -0+190 &

xterm -geometry 80x50+0+0 -e tmux attach-session -d &

exec cwm
