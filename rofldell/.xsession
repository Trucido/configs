# woddfellow2's ~/.xsession (rofldell)
# by woddfellow2 | http://wlair.us.to/

export LC_CTYPE="en_US.UTF-8"

xcompmgr &

xscreensaver -no-splash &
setxkbmap -option "caps:escape" -option "compose:ralt" &

feh --bg-max ~/img/wallpaper/DNA\ -1920\ x\ 1200-597050.jpeg &

xclock -geometry -0+0 &
xload -geometry -0+40 &
xbiff -geometry -0+190 &

urxvt -geometry 80x50+0+0 -e tmux attach-session -d &

exec cwm
